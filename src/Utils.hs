module Utils where

import           Control.Parallel
import           Control.Parallel.Strategies

import           Data.Fixed (divMod', mod')
import           Data.Ord (comparing)
import           Data.List (find, groupBy, minimumBy, sort, sortBy)
import           Data.Maybe (fromMaybe)

import           Data.Time (Day, DiffTime, NominalDiffTime, TimeOfDay(..), LocalTime, ZonedTime(..), fromGregorian, timeToTimeOfDay, timeOfDayToTime, toModifiedJulianDay, zonedTimeToUTC, utcToLocalTime, addUTCTime, diffUTCTime, localTimeToUTC, utc)
import qualified Data.Time as Time

import           GHC.Conc (numCapabilities)

safeHead :: [a] -> Maybe a
safeHead (a:_) = Just a
safeHead _ = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast as = Just $ last as

tagMinWith :: Ord b => (a -> b) -> (a -> c) -> (a -> c) -> [a] -> ([c], a)
tagMinWith value injMin injNotMin as = (map inj zas, mina)
  where
    zas = zip as [0..]
    (mina, tag) = minimumBy (comparing (value . fst)) zas
    inj (a, t) = if t == tag
      then injMin a
      else injNotMin a

findMaybe :: (a -> Maybe b) -> [a] -> Maybe (a, b)
findMaybe _ [] = Nothing
findMaybe f (a:as) = case f a of
  Just r -> Just (a, r)
  Nothing -> findMaybe f as

between :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe [a]
between start end xs = case dropWhile (not . start) xs of
  [] -> Nothing
  rest -> case find end (tail rest) of
    Just _ -> Just $ takeWhileIncl (not . end) rest
    Nothing -> Nothing

  where
    takeWhileIncl :: (a -> Bool) -> [a] -> [a]
    takeWhileIncl _ [] =  []
    takeWhileIncl p (x:xs)
      | p x       = x:takeWhileIncl p xs
      | otherwise = [x]

fconcat :: [[a]] -> [a]
fconcat [] = []
fconcat (as@(_:_):_) = as
fconcat (_:as) = fconcat as

--------------------------------------------------------------------------------

chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys, zs) = splitAt n xs

parmconcat :: NFData a => Monoid a => [a] -> a
parmconcat as = mconcat bs
  where
    parts = chunk numCapabilities as
    bs = map mconcat parts `using` parList rdeepseq

-- Time utils ------------------------------------------------------------------

pico :: Integer
pico = 1000000000000

addSeconds :: Int -> TimeOfDay -> TimeOfDay
addSeconds secs tod
  = snd $ timeToDaysAndTimeOfDay $ daysAndTimeOfDayToTime 0 tod + fromIntegral secs

-- From https://hackage.haskell.org/package/time-1.9/docs/Data-Time-LocalTime.html#v:timeToDaysAndTimeOfDay

-- | Convert a period of time into a count of days and a time of day since midnight.
-- The time of day will never have a leap second.
timeToDaysAndTimeOfDay :: NominalDiffTime -> (Integer,TimeOfDay)
timeToDaysAndTimeOfDay dt = let
    s = realToFrac dt
    (m,ms) = divMod' s 60
    (h,hm) = divMod' m 60
    (d,dh) = divMod' h 24
    in (d,TimeOfDay dh hm ms)

-- | Convert a count of days and a time of day since midnight into a period of time.
daysAndTimeOfDayToTime :: Integer -> TimeOfDay -> NominalDiffTime
daysAndTimeOfDayToTime d (TimeOfDay dh hm ms) = (+) (realToFrac ms) $ (*) 60 $ (+) (realToFrac hm) $ (*) 60 $ (+) (realToFrac dh) $ (*) 24 $ realToFrac d

defaultZTime :: ZonedTime
defaultZTime = Time.ZonedTime
  (Time.LocalTime (Time.fromGregorian 2018 06 06) Time.midnight)
  Time.utc

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime z1 z2 = zonedTimeToUTC z1 `diffUTCTime` zonedTimeToUTC z2

addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc

addZonedTime :: NominalDiffTime -> ZonedTime -> ZonedTime
addZonedTime t (ZonedTime lt tz) = ZonedTime (addLocalTime t lt) tz
