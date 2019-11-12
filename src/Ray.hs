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

import Data.List

import Linear
import Math

data Ray = Ray (V4 Double) (V4 Double) deriving (Show, Eq)

data Intersection = Intersection Double Int deriving (Show, Eq)

data Sphere = Sphere Int (M44 Double)

instance Ord Intersection where
    (Intersection t1 _) `compare` (Intersection t2 _) = t1 `compare` t2

transformRay :: Ray -> M44 Double -> Ray
transformRay (Ray o d) m = Ray (m !* o) ( m !* d) 

-- This assumes your list of intersections is already sorted.
hit :: [Intersection] -> Maybe Intersection
hit (x@(Intersection t _):xs) = if t >= 0.0 then (Just x) else hit xs
hit [] = Nothing

sortIntersections :: [Intersection] -> [Intersection]
sortIntersections xs = sort xs

sphere :: Sphere
sphere = Sphere 1 (identity :: M44 Double)

intersects :: Sphere -> Ray -> [Double]
intersects s r =
    if d < 0
        then 
            []
        else 
            [t1, t2]
    where d = discriminant r
          a = a' r
          b = b' r 
          t1 = (-b - (sqrt d)) / (2 * a)
          t2 = (-b + (sqrt d)) / (2 * a)

discriminant :: Ray -> Double
discriminant (Ray o d) = b^2  - (4 * a * c)
    where sphereToRay = sphereToRay' o
          a = a' (Ray o d)
          b = b' (Ray o d)
          c = (dot sphereToRay sphereToRay) - 1

sphereToRay' o = o - (pnt 0 0 0)

a' :: Ray -> Double
a' (Ray _ d) = dot d d

b' :: Ray -> Double
b' (Ray o d) = 2 * (dot d (sphereToRay' o))

pos :: Ray -> Double -> V4 Double
pos (Ray o d) t = o + (d ^* t)
