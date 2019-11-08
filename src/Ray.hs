module Ray
( 
    Ray(..)
,   Sphere(..)
,   pos
,   intersects'
) where

import Linear
import Math

data Ray = Ray (V4 Double) (V4 Double)

data Intersection = Intersection Double Int

class Intersectable a where
    intersects :: a -> Ray -> [Intersection]

data Sphere = Sphere Int

--instance Intersectable Sphere where
--    intersects = intersects' 

intersects' :: Sphere -> Ray -> [Double]
intersects' s r =
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
