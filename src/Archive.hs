{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS -fno-warn-orphans #-}

module Archive where

import           Control.DeepSeq (NFData)

import           Data.Aeson.Types (FromJSON, ToJSON, ToJSONKey, Value (Object), (.:), (.=), genericParseJSON, defaultOptions, camelTo2, genericToJSON, object, parseJSON, toJSON, fieldLabelModifier)
import           Data.Maybe (fromMaybe, isJust, isNothing)
import           Data.Monoid ((<>))
import qualified Data.Map as M
import           Data.List (zip4)
import           Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.Clock (UTCTime (utctDayTime), diffTimeToPicoseconds)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Time.LocalTime (LocalTime (LocalTime), ZonedTime (ZonedTime), midnight, utc, zonedTimeToLocalTime, localDay)
import           Data.Time (Day, NominalDiffTime, TimeOfDay (TimeOfDay), fromGregorian, timeOfDayToTime, zonedTimeToUTC)
import qualified Data.Time as Time
import           Data.Text (Text)
import           Data.Word (Word8, Word16, Word32, Word64)

import           Lens.Micro (_1, (^.))

import           GHC.Generics (Generic)

import qualified Numeric.Interpolate as I
import qualified Geodesy

import qualified Network.HTTP.Client as HTTP

import           Servant (Proxy (Proxy), QueryParam, JSON, Get, (:>))
import           Servant.Client (ClientM, client, parseBaseUrl, runClientM, mkClientEnv)

import           Utils (pico, addLocalTime, between, diffZonedTime, addZonedTime)

-- Orphan instances ------------------------------------------------------------

instance S.Serialize T.Text where
  get = TE.decodeUtf8 <$> S.get
  put = S.put . TE.encodeUtf8

instance S.Serialize Day where
  get = Time.ModifiedJulianDay <$> S.get
  put = S.put . Time.toModifiedJulianDay

instance S.Serialize TimeOfDay where
  get = Time.timeToTimeOfDay . Time.picosecondsToDiffTime <$> S.get
  put = S.put . Time.diffTimeToPicoseconds . Time.timeOfDayToTime

epoch :: Time.UTCTime
epoch = Time.UTCTime (Time.fromGregorian 0 1 1) (fromIntegral (0 :: Int))

instance S.Serialize ZonedTime where
  get   = do
    t  <- S.get :: S.Get Word64
    tz <- S.get :: S.Get Word16
    pure $ Time.utcToZonedTime (Time.minutesToTimeZone $ fromIntegral tz) (Time.addUTCTime (fromIntegral t) epoch)
  put t = do
    S.put $ convert64 $ round $ Time.diffUTCTime (Time.zonedTimeToUTC t) epoch
    S.put $ convert16 $ fromIntegral $ Time.timeZoneMinutes $ Time.zonedTimeZone t
    where
      convert64 :: Word64 -> Word64
      convert64 = id

      convert16 :: Word16 -> Word16
      convert16 = id

-- TAPI ------------------------------------------------------------------------

newtype Atcocode = Atcocode { getAtcocode :: Text }
  deriving newtype (Eq, Ord, Show, NFData, S.Serialize, ToJSON, FromJSON)

data TAPI_Point = TAPI_Point Double Double deriving (Show, Generic, NFData, S.Serialize)

instance FromJSON TAPI_Point where
  parseJSON (Object o) = do
    [x, y] <- o .: "coordinates"
    pure $ TAPI_Point x y
  parseJSON _ = fail "Expected object"

instance ToJSON TAPI_Point where
  toJSON (TAPI_Point lat lng) = object
    [ "type"        .= ("Point" :: Text)
    , "coordinates" .= [ lat, lng ]
    ]

data TAPI_Stop = TAPI_Stop
  { tstTime      :: TimeOfDay
  , tstDate      :: Text
  , tstAtcocode  :: Atcocode
  , tstName      :: Text
  , tstStopName  :: Text
  , tstSmscode   :: Maybe Text
  , tstLocality  :: Text
  , tstBearing   :: Maybe Text
  , tstIndicator :: Maybe Text
  , tstLatitude  :: Geodesy.Latitude Double
  , tstLongitude :: Geodesy.Longitude Double
  } deriving (Show, Generic, NFData, S.Serialize)

instance FromJSON TAPI_Stop where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    }

instance ToJSON TAPI_Stop where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    }

data TAPI_StopsIndex = TAPI_StopsIndex
  { tsiValue :: Int
  , tsiType  :: Text
  } deriving (Show, Generic, NFData, S.Serialize)

instance FromJSON TAPI_StopsIndex where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    }

instance ToJSON TAPI_StopsIndex where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    }

data TAPI_ProgressBetweenStops = TAPI_ProgressBetweenStops
  { tpbsValue :: Double
  } deriving (Show, Generic, NFData, S.Serialize)

instance FromJSON TAPI_ProgressBetweenStops where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 4
    }

instance ToJSON TAPI_ProgressBetweenStops where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 4
    }

data TAPI_Status = TAPI_Status
  { tstatLocation             :: TAPI_Point
  , tstatStopsIndex           :: TAPI_StopsIndex
  , tstatProgressBetweenStops :: TAPI_ProgressBetweenStops
  , tstatRecordedAtTime       :: ZonedTime
  , tstatBearing              :: Double
  , tstatVehicleId            :: Text
  } deriving (Show, Generic, NFData, S.Serialize)

instance FromJSON TAPI_Status where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 5
    }

instance ToJSON TAPI_Status where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 5
    }

data TAPI_Member = TAPI_Member
  { tmDir            :: Text
  , tmLine           :: Text
  , tmLineName       :: Text
  , tmOperator       :: Text
  , tmOperatorName   :: Text
  , tmOriginAtcocode :: Text
  , tmRequestTime    :: Text
  , tmStops          :: [TAPI_Stop]
  , tmStatus         :: TAPI_Status
  } deriving (Show, Generic, NFData, S.Serialize)

instance FromJSON TAPI_Member where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

instance ToJSON TAPI_Member where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

data TAPI_ServicesTimetables = TAPI_ServicesTimetables
  { tstId     :: Text
  , tstMember :: [TAPI_Member]
  } deriving (Show, Generic)
  
instance FromJSON TAPI_ServicesTimetables where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    }

instance ToJSON TAPI_ServicesTimetables where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 3
    }

type ServicesTimetablesEndpoint
  = "v3" :> "uk" :> "bus" :> "service_timetables.json"
  :> QueryParam "service" Text
  :> QueryParam "operator" Text
  :> QueryParam "direction" Text
  :> QueryParam "active" Text
  :> QueryParam "source_config" Text
  :> QueryParam "live" Text
  :> QueryParam "app_id" Text
  :> QueryParam "app_key" Text
  :> Get '[JSON] TAPI_ServicesTimetables

queryServiceTimetables
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM TAPI_ServicesTimetables
queryServiceTimetables = client (Proxy :: Proxy ServicesTimetablesEndpoint)

testQuery :: T.Text -> T.Text -> IO ()
testQuery appId appKey = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  url <- parseBaseUrl "http://nxwm.transportapi.com"
  r <- runClientM
        (Archive.queryServiceTimetables
           (j "74")
           (j "NXB")
           (j "outbound")
           (j "true")
           (j "nxwm_siri_vm")
           (j "true")
           (j appId)
           (j appKey)) 
        (mkClientEnv manager url)
  writeFile "service_timetables.pp" (show r)
  where
    j = Just

-- Types -----------------------------------------------------------------------

data JourneySegment = JourneySegment
  { jsStart :: TAPI_Stop
  , jsEnd   :: TAPI_Stop
  } deriving (Generic, Show, NFData, S.Serialize)

instance ToJSON JourneySegment where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

journeySegmentEq :: JourneySegment -> JourneySegment -> Bool
journeySegmentEq s1 s2
  =  atcocode jsStart s1 == atcocode jsStart s2
  && atcocode jsEnd s1 == atcocode jsEnd s2
  where
    atcocode f = tstAtcocode . f

type Progress = Double

data MatchedJourneySegment = MatchedJourneySegment
  { msSegment  :: JourneySegment
  , msFraction :: Progress
  } deriving (Generic, Show, NFData, S.Serialize)

instance ToJSON MatchedJourneySegment where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

type PathPoint = (ZonedTime, Progress)

data TravelTime = TravelTime
  { ttService             :: Service

  , ttObservedTravelTime  :: Word16
  , ttScheduledTravelTime :: Word16

  , ttDayOfWeek           :: Word8

  , ttObservedStartTime   :: ZonedTime
  , ttObservedEndTime     :: ZonedTime
  , ttScheduledStartTime  :: TimeOfDay
  , ttScheduledEndTime    :: TimeOfDay

  , ttPath                :: [PathPoint]

  , ttLinkId              :: Word32

  , ttInterpolated        :: Bool
  } deriving (Show, Generic, NFData, S.Serialize)

instance ToJSON TravelTime where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

instance FromJSON TravelTime where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

data Service = Service
  { svLineName      :: Text
  , svOperator      :: Text
  , svDirection     :: Text
  , svDepartureTime :: TimeOfDay
  } deriving (Eq, Ord, Show, Generic, NFData, S.Serialize)

instance ToJSON Service where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

instance FromJSON Service where
  parseJSON = genericParseJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

data Movement = Movement
  { mvFrom         :: Atcocode
  , mvFromLocation :: (Geodesy.Latitude Double, Geodesy.Longitude Double)
  , mvTo           :: Atcocode
  , mvToLocation   :: (Geodesy.Latitude Double, Geodesy.Longitude Double)
  , mvBucket       :: Word8
  } deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON Movement where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

instance ToJSONKey Movement

instance S.Serialize a => S.Serialize (NonEmpty a)

data RecordedSegment = RecordedSegment
  { rsSegment :: JourneySegment
  , rsPath    :: NonEmpty PathPoint
  } deriving (Show, Generic, NFData, S.Serialize)

instance ToJSON RecordedSegment where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

data RecordedJourney = RecordedJourney
  { rjRecordedSegments :: NonEmpty RecordedSegment
  , rjScheduledJourney :: TAPI_Member
  , rjStops            :: [TAPI_Stop]
  , rjLinkId           :: Word32
  } deriving (Show, Generic, NFData, S.Serialize)

instance ToJSON RecordedJourney where
  toJSON = genericToJSON $ defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 2
    }

type JourneyMap  = M.Map Text TAPI_Member
type SegmentMap  = M.Map Text RecordedJourney
type MovementMap = M.Map Movement [TravelTime]

-- Utils -----------------------------------------------------------------------

timeRangeForBucket :: Word8 -> (TimeOfDay, TimeOfDay)
timeRangeForBucket bodFrom =
  ( TimeOfDay hourFrom (fromIntegral bodFrom * 15 - hourFrom * 60) 0
  , TimeOfDay hourTo (bodTo * 15 - hourTo * 60) 0
  )
  where
    bodTo    = fromIntegral bodFrom + 1

    hourFrom = fromIntegral bodFrom `div` 4
    hourTo   = fromIntegral bodTo `div` 4

bucketForTimeOfDay :: TimeOfDay -> Word8
bucketForTimeOfDay (TimeOfDay h m _)
  = fromIntegral ((h * 60 + m) `div` 15)

bucketForTime :: ZonedTime -> Word8
bucketForTime zt
  = fromIntegral
  $ (diffTimeToPicoseconds $ utctDayTime utcTime) `div` (pico * 60 * 15)
  where
    utcTime = zonedTimeToUTC zt

deleteKeys :: Ord k => [k] -> M.Map k v -> M.Map k v
deleteKeys keys m = foldr M.delete m keys

-- Travel times ----------------------------------------------------------------

makeJourneyMap :: [TAPI_Member] -> JourneyMap
makeJourneyMap journeys = M.fromList
  [ (tstatVehicleId $ tmStatus journey, journey)
  | journey <- journeys
  ]

segmentTravelTime :: TAPI_Stop -> TAPI_Stop -> Word16
segmentTravelTime from to = fromIntegral (picoTime `div` pico)
  where
    picoTime = diffTimeToPicoseconds $ (timeOfDayToTime $ tstTime to) - (timeOfDayToTime $ tstTime from)

stepMovements :: [TAPI_Member] -> SegmentMap -> (SegmentMap, MovementMap)
stepMovements journeys segmentMap
  = (newSegmentMap, M.unionsWith (<>) (map snd finishedJourneys))
  where
    finishedJourneys =
      [ (jsId, movementsForRecordedJourney recJourney)
      | Just minTimeForServices' <- [ minTimeForServices ]
      , (jsId, recJourney) <- M.toList segmentMap
      , purgeJourneyTime `addLocalTime` maxTimeForJourney recJourney < minTimeForServices'
      ]

    journeyMap = makeJourneyMap journeys
    purgedSegmentMap = deleteKeys (map fst finishedJourneys) segmentMap

    recSegments =
      [ (jsId, recSegment)
      | (jsId, match) <- M.assocs journeyMap
      , Just recSegment <- [ step match (M.lookup jsId purgedSegmentMap) ]
      ]

    newSegmentMap = M.fromList recSegments `M.union` purgedSegmentMap

    -- 

    purgeJourneyTime :: NominalDiffTime
    purgeJourneyTime = fromInteger (60 * 15)

    -- TODO: don't use partial functions like minimum
    minTimeForServices :: Maybe LocalTime
    minTimeForServices
      | null times = Nothing
      | otherwise  = Just (minimum times)
      where
        times = 
          [ zonedTimeToLocalTime (tstatRecordedAtTime $ tmStatus journey)
          | journey <- journeys
          ]

    maxTimeForPath :: RecordedSegment -> LocalTime
    maxTimeForPath = maximum . map (zonedTimeToLocalTime . (^. _1)) . NE.toList . rsPath

    maxTimeForJourney :: RecordedJourney -> LocalTime
    maxTimeForJourney recJourney = maximum
      [ maxTimeForPath recSegment
      | recSegment <- NE.toList $ rjRecordedSegments recJourney
      ]

journeySegment :: TAPI_Member -> Maybe MatchedJourneySegment
journeySegment journey = Just $ MatchedJourneySegment
  { msSegment  = JourneySegment
      { jsStart = start
      , jsEnd   = end
      }
  , msFraction = tpbsValue $ tstatProgressBetweenStops $ tmStatus journey
  }
  where
    index = tsiValue (tstatStopsIndex $ tmStatus journey)
    (start, end) = zip (tmStops journey) (tail $ tmStops journey) !! index

distanceBetweenStops :: TAPI_Stop -> TAPI_Stop -> Double
distanceBetweenStops from to = Geodesy.distance
  (Geodesy.Location (tstLatitude from) (tstLongitude from))
  (Geodesy.Location (tstLatitude to) (tstLongitude to))

step :: TAPI_Member -> Maybe RecordedJourney -> Maybe RecordedJourney
step journey recJourney
  | Just newSegment <- journeySegment journey = case recJourney of
      -- New match (case 1)
      Nothing -> Just $ RecordedJourney
        { rjRecordedSegments = singleton $ newRecSegment newSegment
        , rjScheduledJourney = journey
        , rjStops = tmStops journey
        , rjLinkId = floor (utcTimeToPOSIXSeconds $ zonedTimeToUTC $ tstatRecordedAtTime $ tmStatus journey)
        }

      -- Existing match (case 1, 1.1)
      Just oldRecJourney
        -- Same segment (case 1), add path point to current segment
        | currentSegment oldRecJourney `journeySegmentEq` msSegment newSegment -> Just $ oldRecJourney
            { rjRecordedSegments = mapFirst (rjRecordedSegments oldRecJourney) $ \rs -> rs
                { rsPath = pathPoint (msFraction newSegment) <| rsPath rs
                }
            }

        -- New segment (case 1, 1.1); check if matched segment is indeed
        -- further along the route compared to the latest matched segment
        | stopAfter
            (rjStops oldRecJourney)
            (jsStart $ rsSegment $ NE.head $ rjRecordedSegments oldRecJourney)
            (jsEnd $ msSegment newSegment) -> Just $ oldRecJourney
              { rjRecordedSegments = newRecSegment newSegment <| rjRecordedSegments oldRecJourney
              }

        -- Matched segment is an earlier segment compared to the
        -- latest matched segment; reject
        | otherwise -> recJourney

  -- Unmatched segment (case 1.2)
  | otherwise = recJourney
  where
    pathPoint fraction = (tstatRecordedAtTime $ tmStatus journey, fraction)

    newRecSegment newSegment = RecordedSegment
      { rsSegment = msSegment newSegment
      , rsPath = singleton (pathPoint $ msFraction newSegment)
      }

    currentSegment = rsSegment . NE.head . rjRecordedSegments

    stopAfter stops stop after
      = isJust
      $ between
          (\s -> tstAtcocode s == tstAtcocode stop)
          (\s -> tstAtcocode s == tstAtcocode after)
          stops

    --

    singleton a = a :| []
    mapFirst ne f = f (NE.head ne) :| (NE.tail ne)

-- | "Smooths out" small invalid gaps in a sequence as determined by
-- the predicate @f@.
--
-- I.e. if @*@ stands for "valid" and @-@ for "invalid":
--
-- @
-- [*, -, *, *]    -> [*, *, *, *]
-- [*, -, *, -, *] -> [*, *, *, *, *]
-- @
--
-- But:
--
-- @
-- [*, -, -, *, -, *] -> [*, -, -, *, *, *]
-- @
smooth :: (a -> Bool) -> [a] -> [(a, Bool)]
smooth _ [] = []
smooth f (a:b:c:as)
  | f a && f c = (a, True):(b, True):smooth f (c:as)
smooth f (a:as) = (a, f a):smooth f as

movementsForRecordedJourney :: RecordedJourney -> MovementMap
movementsForRecordedJourney recJourney = movements
  where
    stopsBetween stop end
      = between
          (\s -> tstAtcocode s == tstAtcocode stop)
          (\s -> tstAtcocode s == tstAtcocode end)

    stops = fromMaybe [] $ stopsBetween
      (jsStart $ rsSegment $ NE.last $ rjRecordedSegments recJourney)
      (jsEnd $ rsSegment $ NE.head $ rjRecordedSegments recJourney)
      (rjStops recJourney)

    zeroday = ZonedTime (LocalTime (fromGregorian 0 1 1) midnight) utc

    pointsForPaths :: [(Double, [PathPoint])] -> [(Double, NominalDiffTime)]
    pointsForPaths paths = concat
      [ [ (f * distance + runningDistance, t `diffZonedTime` zeroday)
        | (t, f) <- path
        ]
      | ((distance, path), runningDistance) <- zip
          paths
          (scanl (+) 0 [ distance | (distance, _) <- paths ])
      ]

    segments =
      [ ((start, end), distanceBetweenStops start end)
      | (start, end) <- zip stops (tail stops)
      ]

    segmentMap = M.fromList
      [ ( ( tstAtcocode $ jsStart $ rsSegment recSegment
          , tstAtcocode $ jsEnd $ rsSegment recSegment
          )
        , recSegment
        )
      | recSegment <- NE.toList $ rjRecordedSegments recJourney
      ]

    points = pointsForPaths
      [ (distance, maybe [] (NE.toList . rsPath) (lookupSegment segment))
      | segment@(_, distance) <- segments
      ]

    runningDistances = scanl (+) 0 [ distance | (_, distance) <- segments ]
    times = map ((`addZonedTime` zeroday) <$>) $ I.interpolate points runningDistances

    lookupSegment ((start, end), _) = M.lookup (tstAtcocode start, tstAtcocode end) segmentMap

    movements = M.fromListWith (<>)
      [ ( Movement
            { mvFrom = tstAtcocode start
            , mvFromLocation =
                ( tstLatitude start
                , tstLongitude start
                )
            , mvTo = tstAtcocode end
            , mvToLocation =
                ( tstLatitude end
                , tstLongitude end
                )
            , mvBucket = bucketForTime startTime
            }
        , [ TravelTime
              { ttService = Service
                  { svLineName = tmLineName journey
                  , svOperator = tmOperatorName journey
                  , svDirection = tmDir journey
                  , svDepartureTime = tstTime $ head $ tmStops journey
                  }

              , ttObservedTravelTime = round (endTime `diffZonedTime` startTime)
              , ttScheduledTravelTime = segmentTravelTime start end

              , ttDayOfWeek = fromIntegral dayOfWeek

              , ttObservedStartTime = startTime
              , ttObservedEndTime = endTime
              , ttScheduledStartTime = tstTime start
              , ttScheduledEndTime = tstTime end

              , ttPath = maybe [] (NE.toList . rsPath) recSegment

              , ttLinkId = rjLinkId recJourney

              , ttInterpolated = isNothing recSegment
              }
          ]
        )
      | ((_, invalid), ((start, end), _), Just startTime, Just endTime) <- zip4
          (smooth (not . snd) $ smooth (isJust . lookupSegment) segments)
          segments
          times
          (tail times)

      , not invalid

      , let journey = rjScheduledJourney recJourney
            recSegment = M.lookup (tstAtcocode start, tstAtcocode end) segmentMap
            (_, _, dayOfWeek) = toWeekDate $ localDay $ zonedTimeToLocalTime startTime
      ]
