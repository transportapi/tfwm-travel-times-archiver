{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS -fno-warn-orphans #-}

module API where

import           Control.Arrow ((***))
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (threadDelay)
import           Control.DeepSeq (force)
import           Control.Exception (evaluate)

import           Data.Aeson (encode, eitherDecode)
import           Data.Aeson.Types (FromJSON, ToJSON, parseJSON, genericParseJSON, toJSON, genericToJSON, defaultOptions, fieldLabelModifier, camelTo2)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.MonoidMap (type (~~>), (~~>))
import qualified Data.MonoidMap as MM
import qualified Data.Map as M
import           Data.List (isSuffixOf, sort, sortBy)
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Serialize as S
import           Data.SortedList (SortedList)
import qualified Data.SortedList as SL
import           Data.Time.LocalTime (ZonedTime)
import           Data.Time (TimeOfDay)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Data.Word (Word8, Word16)

import           GHC.Generics (Generic)

import qualified Database.Query as Q

import           Archive (Atcocode (Atcocode))
import qualified Archive

import qualified Network.HTTP.Client as HTTP
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors (simpleCors)

import qualified Database.LevelDB as LDB

import           Servant (Application, Capture, Get, JSON, (:>), errBody, serve, throwError, err400)
import           Servant.Client (mkClientEnv, runClientM, parseBaseUrl)

import           System.Directory (getDirectoryContents)

import           Debug.Trace (traceIO)

median :: Ord a => [a] -> Maybe a
median [] = Nothing
median as = Just $ sort as !! (length as `div` 2)

medianBy :: (a -> a -> Ordering) -> [a] -> Maybe a
medianBy _ [] = Nothing
medianBy f as = Just $ sortBy f as !! (length as `div` 2)

-- Schema ----------------------------------------------------------------------

instance Q.ToByteString Atcocode where
  toByteString (Atcocode atcocode) = TE.encodeUtf8 atcocode <> ":"

instance Q.FromByteString Atcocode where
  parseByteString = Atcocode . TE.decodeUtf8 <$> go ""
    where
      go bs = do
        c <- S.get
        if c /= ':'
          then go (BC.snoc bs c)
          else pure bs

instance Q.ToByteString Word8 where
  toByteString = S.encode

instance Q.FromByteString Word8 where
  parseByteString = S.get

instance Q.ToByteString Int where
  toByteString = S.encode

instance Q.FromByteString Int where
  parseByteString = S.get

instance Q.ToByteString ZonedTime where
  toByteString = S.encode

instance Q.FromByteString ZonedTime where
  parseByteString = S.get

type MovementKeyP = (Atcocode, Atcocode, ZonedTime, Word8, Int)
type MovementKey  = Q.Key '[Atcocode, Atcocode, ZonedTime, Word8, Int]

movementsTable :: Q.Table MovementKey Archive.TravelTime
movementsTable = Q.Table "m"

type AtcocodePairKey = Q.Key '[Atcocode, Atcocode]

pairsTable :: Q.Table AtcocodePairKey ()
pairsTable = Q.Table "p"

segmentsTable :: Q.Table (Q.Key '[]) Archive.SegmentMap
segmentsTable = Q.Table "s"

--------------------------------------------------------------------------------

data Config = Config
  { cfgUrl      :: Maybe String
  , cfgSource   :: Maybe T.Text
  , cfgAppId    :: T.Text
  , cfgAppKey   :: T.Text
  , cfgServices :: [(T.Text, [T.Text])]
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    }

data ServiceState = ServiceState
  { ssMovementMap :: Archive.MovementMap
  }

scrape :: LDB.DB -> Config -> IO ()
scrape ldb cfg = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  url     <- parseBaseUrl (fromMaybe "http://nxwm.transportapi.com" $ cfgUrl cfg)

  values  <- Q.query ldb segmentsTable (Q.fromTuple ())

  segmentMap <- pure $ case values of
    [(_, segmentMap)] -> segmentMap
    _ -> M.empty

  go (mkClientEnv manager url) segmentMap

  where
    go env segmentMap = do
      members' <- sequence
        [ runClientM
            (Archive.queryServiceTimetables
               (j service)
               (j operator)
               (j direction)
               (j "true")
               (j $ fromMaybe "nxwm_siri_vm" $ cfgSource cfg)
               (j "true")
               (j $ cfgAppId cfg)
               (j $ cfgAppKey cfg)) 
            env
        | (operator, services) <- cfgServices cfg
        , direction <- directions
        , service <- services
        ]

      segmentMap' <- case sequence members' of
        Left e -> do
          traceIO $ show e
          pure segmentMap
        Right members -> do
          (segmentMap', movementMap) <- evaluate
            $ force
            $ Archive.stepMovements (concatMap Archive.tstMember members) segmentMap

          writeServiceStateToLevelDB ldb segmentMap' movementMap
          pure segmentMap'

      threadDelay (20 * 1000 * 1000)
      go env segmentMap'

    directions = [ "inbound", "outbound" ]

    j = Just

replay :: LDB.DB -> IO ()
replay ldb = do
  files <- filter (isSuffixOf "json") . sort <$> getDirectoryContents path

  go files M.empty
  where
    path = "data/scrape/in/"

    go [] _ = pure ()
    go (file:files) segmentMap = do
      traceIO $ "Reading " <> file <> "..."
      members' <- eitherDecode <$> BL.readFile (path <> file)

      let root = "data/scrape/out/" <> file

      segmentMap' <- case (members' :: Either String [Archive.TAPI_ServicesTimetables]) of
        Left e -> do
          traceIO e
          pure segmentMap
        Right members -> do
          BL.writeFile (root <> ".response.json") $ encode members

          (segmentMap', movementMap) <- evaluate
            $ force
            $ Archive.stepMovements (concatMap Archive.tstMember members) segmentMap

          BL.writeFile (root <> ".segments.json") $ encode segmentMap'
          BL.writeFile (root <> ".movements.json") $ encode movementMap

          writeServiceStateToLevelDB ldb segmentMap' movementMap
          pure segmentMap'

      go files segmentMap'

writeServiceStateToLevelDB
  :: LDB.DB
  -> Archive.SegmentMap
  -> Archive.MovementMap
  -> IO ()
writeServiceStateToLevelDB ldb segmentMap movementMap =
  LDB.write ldb LDB.defaultWriteOptions (movementMapBatch <> segmentMapBatch)
  where
    movementMapBatch = concat 
        [ [ Q.putOp movementsTable (movementToKey movement travelTime seqId) travelTime
          , Q.putOp pairsTable (Q.fromTuple (Archive.mvFrom movement, Archive.mvTo movement)) ()
          ]
        | ((movement, travelTimes), seqId) <- zip (M.toList movementMap) [0..]
        , travelTime <- travelTimes
        ]

    segmentMapBatch = [ Q.putOp segmentsTable (Q.fromTuple ()) segmentMap ]

    movementToKey movement travelTime seqId = Q.fromTuple
      ( Archive.mvFrom movement
      , Archive.mvTo movement
      , Archive.ttObservedStartTime travelTime
      , Archive.mvBucket movement
      , seqId
      )

collectTravelTimes
  :: ZonedTime
  -> ZonedTime
  -> TimeOfDay
  -> TimeOfDay
  -> [Bool]
  -> LDB.DB
  -> IO ((T.Text, T.Text) ~~> Word8 ~~> (SortedList Word16, SortedList Word16))
collectTravelTimes dayStart dayEnd todStart todEnd weekdays ldb = do
  ps <- Q.keysForTable ldb pairsTable
  tts <- mconcat <$> sequence
    [ Q.between
        ldb
        movementsTable
        (Q.fromTuple (from, to, dayStart))
        (Q.fromTuple (from, to, dayEnd))
    | (from, to) <- map Q.toTuple ps
    ]

  let filteredTts = filteredTravelTimes tts

  pure (aggregate filteredTts)

  where
    filteredTravelTimes travelTimes =
      [ tt
      | tt@((_, _, _, bucket, _), travelTime) <- map (first Q.toTuple) travelTimes
      ,  if todEnd > todStart
           then bucket >= Archive.bucketForTimeOfDay todStart
             && bucket <= Archive.bucketForTimeOfDay todEnd
           else bucket >= Archive.bucketForTimeOfDay todStart
             || bucket <= Archive.bucketForTimeOfDay todEnd
      , weekdays !! (fromIntegral (Archive.ttDayOfWeek travelTime) - 1)
      ]

    aggregate
      :: [(MovementKeyP, Archive.TravelTime)]
      -> ((T.Text, T.Text) ~~> Word8 ~~> (SortedList Word16, SortedList Word16))
    aggregate travelTimes = mconcat
      [ (Archive.getAtcocode from, Archive.getAtcocode to)
           ~~> bucket
           ~~> ( SL.singleton (Archive.ttObservedTravelTime travelTime)
               , SL.singleton (Archive.ttScheduledTravelTime travelTime)
               )
      | ((from, to, _, bucket, _), travelTime) <- travelTimes
      ]

medianTravelTimes
  :: ((T.Text, T.Text) ~~> Word8 ~~> (SortedList Word16, SortedList Word16))
  -> ((T.Text, T.Text) ~~> Word8 ~~> (Maybe Word16, Maybe Word16))
medianTravelTimes = fmap $ fmap (median' *** median')
  where
    median' = median . SL.fromSortedList

--------------------------------------------------------------------------------

type TravelTimesAPI
  =  "travel-times"
  :> Capture "from" ZonedTime
  :> Capture "to" ZonedTime
  :> Capture "tod_from" TimeOfDay
  :> Capture "tod_to" TimeOfDay
  :> Capture "weekdays" String
  :> Get '[JSON] TravelTimesRsp

data TravelTimes = TravelTimes
  { ttTimeRange               :: (TimeOfDay, TimeOfDay)
  , ttObservedTravelTimeSecs  :: Word16
  , ttScheduledTravelTimeSecs :: Word16
  } deriving Generic

instance ToJSON TravelTimes where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

data TravelTimesSegment = TravelTimesSegment
  { ttsFrom              :: T.Text
  , ttsTo                :: T.Text
  , ttsMedianTravelTimes :: [TravelTimes]
  } deriving Generic

instance ToJSON TravelTimesSegment where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    }

data TravelTimesRsp = TravelTimesRsp
  { ttrspSegments :: [TravelTimesSegment]
  } deriving Generic

instance ToJSON TravelTimesRsp where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 5
    }

travelTimesApp
  :: LDB.DB
  -> Application
travelTimesApp ldb = serve (Proxy :: Proxy TravelTimesAPI) travelTimesAPI
  where
    travelTimesAPI from to todFrom todTo weekdaysReq
      | Just weekdays <- stringToWeekdays weekdaysReq = do
          travelTimes <- liftIO $ collectTravelTimes from to todFrom todTo weekdays ldb
          pure $ TravelTimesRsp
            { ttrspSegments =
                [ TravelTimesSegment
                    { ttsFrom = from'
                    , ttsTo   = to'
                    , ttsMedianTravelTimes =
                        [ TravelTimes
                           { ttTimeRange               = Archive.timeRangeForBucket bucket
                           , ttObservedTravelTimeSecs  = observed
                           , ttScheduledTravelTimeSecs = scheduled
                           }
                        | (bucket, (Just observed, Just scheduled)) <- MM.toList tts
                        ]
                    }
                | ((from', to'), tts) <- MM.toList $ medianTravelTimes travelTimes
                ]
            }
      | otherwise = throwError $ err400 { errBody = "Wrong weekdays format" }

    charToWeekday 't' = Just True
    charToWeekday 'f' = Just False
    charToWeekday _   = Nothing

    stringToWeekdays :: String -> Maybe [Bool]
    stringToWeekdays str
      | length str /= 7 = Nothing
      | otherwise = sequence $ map charToWeekday str

travelTimesServer :: LDB.DB -> Int -> IO ()
travelTimesServer ldb port = run port $ simpleCors $ travelTimesApp ldb
