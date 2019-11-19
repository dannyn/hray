module Ray
(
  Ray(..)
, Intersection(..)
, Sphere(..)
, transformRay
, hit
, sortIntersections
, sphere
, pos
, intersects
) where

import           Data.List

import           Linear
import           Math

data Ray = Ray { origin    :: V4 Double
               , direction :: V4 Double } deriving (Show)

data Intersection = Intersection { time   :: Double
                                 , object :: Int } deriving (Show, Eq)

data Sphere = Sphere Int (M44 Double)

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

sphere :: Sphere
sphere = Sphere 1 (identity :: M44 Double)

intersects :: Sphere -> Ray -> [Double]
intersects s@(Sphere _ m) r =
    if d < 0
        then
            []
        else
            [t1, t2]
    where tR = transformRay r (inv44 m)
          d = discriminant tR
          a = a' tR
          b = b' tR
          t1 = (-b - sqrt d) / (2 * a)
          t2 = (-b + sqrt d) / (2 * a)

discriminant :: Ray -> Double
discriminant (Ray o d) = b^2  - (4 * a * c)
    where sphereToRay = sphereToRay' o
          a = a' (Ray o d)
          b = b' (Ray o d)
          c = dot sphereToRay sphereToRay - 1

sphereToRay' o = o - pnt 0 0 0

a' :: Ray -> Double
a' (Ray _ d) = dot d d

b' :: Ray -> Double
b' (Ray o d) = 2 * dot d (sphereToRay' o)

pos :: Ray -> Double -> V4 Double
pos (Ray o d) t = o + (d ^* t)
