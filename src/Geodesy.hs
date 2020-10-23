{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Geodesy where

import           Control.DeepSeq (NFData)

import qualified Data.Aeson as A
import qualified Data.Serialize as S
import           Data.Fixed (mod')

import           GHC.Generics (Generic)

newtype Longitude a = Longitude { getLongitude :: a }
  deriving stock (Functor)
  deriving newtype (Num, Eq, Ord, Show, NFData, S.Serialize, A.ToJSON, A.FromJSON)

newtype Latitude a = Latitude { getLatitude :: a }
  deriving stock (Functor)
  deriving newtype (Num, Eq, Ord, Show, NFData, S.Serialize, A.ToJSON, A.FromJSON)

data Location t = Location (Latitude t) (Longitude t)
  deriving (Eq, Ord, Functor, Show, Generic, NFData)

loc :: Double -> Double -> Location Double
loc lat long = Location (Latitude lat) (Longitude long)

-- http://www.movable-type.co.uk/scripts/latlong.html

radius :: Floating a => a
radius = 6371008

toRadians :: Floating a => a -> a
toRadians deg = deg * pi / 180.0

toDegrees :: Floating a => a -> a
toDegrees rad = rad * 180.0 / pi

distance :: RealFloat t => Location t -> Location t -> t
distance (Location (Latitude lat1) (Longitude long1)) (Location (Latitude lat2) (Longitude long2))
  = radius * c
  where
    phi1 = toRadians lat1
    lam1 = toRadians long1

    phi2 = toRadians lat2
    lam2 = toRadians long2

    delta_phi = phi2 - phi1
    delta_lam = lam2 - lam1

    a = sin (delta_phi / 2) * sin (delta_phi / 2)
      + cos phi1 * cos phi2
      * sin (delta_lam / 2) * sin (delta_lam / 2)
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))

bearingTo
  :: RealFloat t
  => Location t
  -> Location t
  -> t
bearingTo
  (Location (Latitude lat1) (Longitude long1))
  (Location (Latitude lat2) (Longitude long2)) = (toDegrees rho + 360) `mod'` 360
  where
    phi1 = toRadians lat1
    phi2 = toRadians lat2
    delta_lam = toRadians (long2 - long1)

    y = sin delta_lam * cos phi2
    x = cos phi1 * sin phi2 - sin phi1 * cos phi2 * cos delta_lam
    rho = atan2 y x

alongTrackDistanceTo
  :: RealFloat t
  => Location t
  -> Location t
  -> Location t
  -> t
alongTrackDistanceTo start end p = delta_at * signum (cos (rho12 - rho13)) * radius
  where
    delta13 = distance start p / radius
    rho13 = toRadians (bearingTo start p)
    rho12 = toRadians (bearingTo start end)

    delta_xt = asin (sin delta13 * sin (rho13 - rho12))
    delta_at = acos (cos delta13 / abs (cos delta_xt))
