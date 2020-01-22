module Scene
(
  Scene(..)
, scene
, intersects
, traceScene
, Sphere(..)
, unitSphere
, sphere
) where

import           Data.List

import           Linear
import           Math
import           Colour
import           Ray

data Shape = Shape { getInts :: Ray -> [Intersection] } 

sphere :: Shape
sphere = Shape (getIntersections s)
         where  s = unitSphere


data Scene = Scene { shape :: Shape 
                   , ray_origin :: V4 Double
                   , canvas_pixels :: Int
                   , wall_size :: Int
                   , wall_z :: Double }

scene = Scene sphere (pnt 0 0 (-5)) 100 7 10

pixelSize :: Scene -> Double
pixelSize (Scene _ _ cp ws _) = (fromIntegral ws) / (fromIntegral cp)

half :: Scene -> Double
half (Scene _ _ _ ws _) = (fromIntegral ws) / 2

worldX :: Scene -> Int -> Double
worldX s x = h + px
    where h = - (half s)
          px = (pixelSize s) * (fromIntegral x)

worldY :: Scene -> Int -> Double
worldY s y =  h - py
    where h = half s
          py = (pixelSize s) * (fromIntegral y)  

getRay :: Scene -> (Int, Int) -> Ray
getRay s@(Scene _ ro _ _ wz) (x, y)= Ray ro (normalize $ pos - ro)
    where pos = pnt (worldX s x) (worldY s y) wz

traceScene:: Scene -> (Int, Int) -> Colour
traceScene s@(Scene (Shape getInts) _ _ _ _ ) (x, y) = getColour $ hit xs
    where ray_origin = pnt 0 0 (-5)
          r = getRay s (x,y)
          xs = getInts r

getColour :: Maybe Intersection -> Colour
getColour (Just (Intersection _ _)) = colour 1.0 0 0
getColour Nothing = colour 0 0 0

------------------------------------------------------------------------------------
data Sphere = Sphere Int (M44 Double)
unitSphere :: Sphere
unitSphere = Sphere 1 (identity :: M44 Double)

getIntersections :: Sphere -> Ray -> [Intersection]
getIntersections s r = sortIntersections [Intersection t 1 | t <- intersects s r]

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
