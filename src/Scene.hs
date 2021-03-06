module Scene
(
  Scene(..)
, Material(..)
, Light(..)
, intersects
, traceScene
, lighting
, Sphere(..)
, unitSphere
, Intersection(..)
, hit
, sortIntersections
, sphere
, normal'
) where

import           Data.List

import           Colour
import           Linear
import           Math
import           Ray

data Shape = Shape { getInts :: Ray -> [Intersection] }

data Material = Material { c' :: Colour
                         , ambient' :: Double
                         , diffuse' :: Double
                         , specular' :: Double
                         , shininess' ::Double }

data Scene = Scene { shape         :: Shape
                   , ray_origin    :: V4 Double
                   , canvas_pixels :: Int
                   , wall_size     :: Int
                   , wall_z        :: Double }

data Intersection = Intersection { time :: Double
                                 , ray :: Ray
                                 , normal :: V4 Double -> V4 Double
                                 , mat :: Material } 

instance Eq Intersection where
    (==) (Intersection t1 _ _ _) (Intersection t2 _ _ _) = dblCmp t1 t2

instance Ord Intersection where
    (Intersection t1 _ _ _) `compare` (Intersection t2 _ _ _) = t1 `compare` t2

instance Show Intersection where
  show (Intersection t _ _ _) = show t

sortIntersections :: [Intersection] -> [Intersection]
sortIntersections = sort

-- This assumes your list of intersections is already sorted.
hit :: [Intersection] -> Maybe Intersection
hit (x@(Intersection t _ _ _):xs) = if t >= 0.0 then Just x else hit xs
hit []                            = Nothing


pixelSize :: Scene -> Double
pixelSize (Scene _ _ cp ws _) = fromIntegral ws / fromIntegral cp

half :: Scene -> Double
half (Scene _ _ _ ws _) = fromIntegral ws / 2

worldX :: Scene -> Int -> Double
worldX s x = h + px
    where h = -(half s)
          px = pixelSize s * fromIntegral x

worldY :: Scene -> Int -> Double
worldY s y =  h - py
    where h = half s
          py = pixelSize s * fromIntegral y

getRay :: Scene -> (Int, Int) -> Ray
getRay s@(Scene _ ro _ _ wz) (x, y)= Ray ro (normalize $ pos - ro)
    where pos = pnt (worldX s x) (worldY s y) wz

traceScene:: Scene -> (Int, Int) -> Colour
traceScene s@(Scene (Shape getInts) _ _ _ _ ) (x, y) = getColour $ hit xs
    where ray_origin = pnt 0 0 (-5)
          r = getRay s (x,y)
          xs = getInts r

getColour :: Maybe Intersection -> Colour
getColour (Just (Intersection t r@(Ray _ rd)  n m)) = lighting m l p rd (n p)
    where p = pos r t
          l = Light (pnt (-10) 10 (-10)) (colour 1 1 1)
getColour Nothing                       = colour 0 0 0

data Light = Light { p :: V4 Double 
                   , i :: Colour }

--- the dot function seems to lack accuracy 
-- material light position eye normal
lighting :: Material -> Light -> V4 Double -> V4 Double -> V4 Double -> Colour
lighting m@(Material mc ma md ms msh) l@(Light lp li) p e n = a + d + s
    where ecolour = mc * li   -- i am iffy about this
          lightv = normalize (lp - p)
          a = ambient m l
          light_dot_normal = dot lightv n
          d = diffuse light_dot_normal ecolour md
          reflectv = reflect (-lightv) n
          reflect_dot_eye = dot reflectv e
          s = specular light_dot_normal reflect_dot_eye li ms msh

effectiveColour :: Material -> Light -> Colour
effectiveColour (Material mc _ _ _ _) (Light _ li) = mc * li

ambient :: Material -> Light -> Colour
ambient m@(Material _ ma _ _ _) l = (effectiveColour m l) ^* ma

-- light_dot_normal effective_colour diffuse
diffuse :: Double -> Colour -> Double -> Colour
diffuse ldn ec md
    | ldn < 0 = colour 0 0 0
    | otherwise = ec ^* (md * ldn)

-- light_dot_normal reflect_dot_eye li specular shininess
specular :: Double -> Double -> Colour -> Double -> Double -> Colour
specular ldn rde li ms msh
    | ldn < 0 = colour 0 0 0
    | otherwise = specular'' rde li ms msh

specular'' :: Double -> Colour -> Double -> Double -> Colour
specular'' rde li ms msh
    | rde <= 0 = colour 0 0 0
    | otherwise = li ^* (ms * factor)
    where factor = rde ** msh

------------------------------------------------------------------------------------
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

