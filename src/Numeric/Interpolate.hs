module Numeric.Interpolate where

import Data.List (find, sortBy)
import Data.Ord (comparing)

interpolate :: (Ord x, Real x, Fractional y, Real y) => [(x, y)] -> [x] -> [Maybe y]
interpolate points xs
  | []       <- sorted = []
  | [(_, y)] <- sorted = [Just y]
  | otherwise          = [ interpolate' (segmentFor x) x | x <- xs ]
  where
    sorted   = monotonically $ sortBy (comparing fst) points
    segments = zip sorted (tail sorted)

    segmentFor x = find (\((x1, _), (x2, _)) -> x >= x1 && x < x2) segments 

    monotonically [] = []
    monotonically [a] = [a]
    monotonically (a@(_, y1):b@(x2, y2):rs)
      | y2 < y1   = a:monotonically ((x2, y1):rs)
      | otherwise = a:monotonically (b:rs)

    interpolate' (Just ((x1, y1), (x2, y2))) x
      | abs dx21 < 0.001 = Nothing
      | otherwise        = Just $
          fromRational (toRational y1 + toRational (y2 - y1) / dx21 * dx1)
      where
        dx21 = toRational (x2 - x1)
        dx1  = toRational (x - x1)
    interpolate' Nothing _ = Nothing
