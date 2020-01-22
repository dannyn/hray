module Ray
(
  Ray(..)
, Intersection(..)
, transformRay
, hit
, sortIntersections
, pos
) where

import           Data.List

import           Linear
import           Math
import           Colour

data Ray = Ray { origin    :: V4 Double
               , direction :: V4 Double } deriving (Show)

data Intersection = Intersection { time   :: Double
                                 , object :: Int } deriving (Show, Eq)

instance Ord Intersection where
    (Intersection t1 _) `compare` (Intersection t2 _) = t1 `compare` t2

instance Eq Ray where
    (==) (Ray o1 d1) (Ray o2 d2) = vecCmp o1 o2 && vecCmp d1 d2

transformRay :: Ray -> M44 Double -> Ray
transformRay (Ray o d) m = Ray (m !* o) ( m !* d)

-- This assumes your list of intersections is already sorted.
hit :: [Intersection] -> Maybe Intersection
hit (x@(Intersection t _):xs) = if t >= 0.0 then Just x else hit xs
hit []                        = Nothing

sortIntersections :: [Intersection] -> [Intersection]
sortIntersections = sort

pos :: Ray -> Double -> V4 Double
pos (Ray o d) t = o + (d ^* t)
