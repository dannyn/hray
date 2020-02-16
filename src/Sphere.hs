module Sphere
(
  Sphere(..)
, unitSphere
, sphere
, intersects
, normal'
) where

import           Data.List

import           Colour
import           Linear
import           Math
import           Ray
import           Scene

data Sphere = Sphere Material (M44 Double)

unitSphere :: Sphere
unitSphere = Sphere defMat (identity :: M44 Double)
    where defMat = Material (colour 1 1 1) 0.1 0.9 0.9 200.0

sphere :: Sphere -> Shape
sphere s = Shape (getIntersections s)

getIntersections :: Sphere -> Ray -> [Intersection]
getIntersections s@(Sphere m _) r = sortIntersections [Intersection t r n m | t <- intersects s r]
    where shp = sphere s
          n = normal' s 

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

--- the pnt here is the origin of the sphere
normal' :: Sphere -> V4 Double -> V4 Double
normal' (Sphere _ m) p = normalize n
    where inv = inv44 m
          object_point = inv !* p
          object_normal = object_point - pnt 0 0 0
          world_normal = Linear.transpose inv !* object_normal
          n = zeroW world_normal 

