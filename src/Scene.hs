module Scene
(
  Shape(..)
, Material(..)
, Scene(..)
, Intersection(..)
, Light(..)
, IntComps(..)
, prepareComps
, traceScene
, lighting
, intersectShapes
, hit
, sortIntersections
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

data Scene = Scene { shape         :: [Shape]
                   , light         :: Light
                   , ray_origin    :: V4 Double
                   , canvas_pixels :: Int
                   , wall_size     :: Int
                   , wall_z        :: Double }

data Intersection = Intersection { time :: Double
                                 , ray :: Ray
                                 , normal :: V4 Double -> V4 Double
                                 , mat :: Material } 

data IntComps = IntComps { icTime :: Double
                         , icPos :: V4 Double
                         , icEye :: V4 Double
                         , icNormal :: V4 Double
                         , icInside :: Bool } deriving (Show)

data Light = Light { p :: V4 Double 
                   , i :: Colour }

instance Eq Intersection where
    (==) (Intersection t1 _ _ _) (Intersection t2 _ _ _) = dblCmp t1 t2

instance Ord Intersection where
    (Intersection t1 _ _ _) `compare` (Intersection t2 _ _ _) = t1 `compare` t2

instance Show Intersection where
  show (Intersection t _ _ _) = show t

instance Eq IntComps where
    (==) (IntComps t1 p1 e1 n1 i1) (IntComps t2 p2 e2 n2 i2) = t && p && e && n && i
        where t = nearZero (t1 - t2)
              p = vecCmp p1 p2
              e = vecCmp e1 e2
              n = vecCmp n1 n2
              i = i1 == i2

sortIntersections :: [Intersection] -> [Intersection]
sortIntersections = sort

intersectShapes :: [Shape] -> Ray -> [Intersection]
intersectShapes shapes r = sort $ getInts r =<< shapes
    where getInts = \r (Shape gi) -> gi r 

-- This assumes your list of intersections is already sorted.
hit :: [Intersection] -> Maybe Intersection
hit (x@(Intersection t _ _ _):xs) = if t >= 0.0 then Just x else hit xs
hit []                            = Nothing

prepareComps :: Intersection -> IntComps
prepareComps (Intersection t r@(Ray _ d) n m) = IntComps t p (-d) normal' inside
    where p = pos r t
          normal = n p
          dotN = dot normal (-d)
          inside = if dotN < 0 then True else False
          normal' = if dotN < 0 then -normal else normal

pixelSize :: Scene -> Double
pixelSize (Scene _ _ _ cp ws _) = fromIntegral ws / fromIntegral cp

half :: Scene -> Double
half (Scene _ _ _ _ ws _) = fromIntegral ws / 2

worldX :: Scene -> Int -> Double
worldX s x = h + px
    where h = -(half s)
          px = pixelSize s * fromIntegral x

worldY :: Scene -> Int -> Double
worldY s y =  h - py
    where h = half s
          py = pixelSize s * fromIntegral y

getRay :: Scene -> (Int, Int) -> Ray
getRay s@(Scene _ _ ro _ _ wz) (x, y)= Ray ro (normalize $ pos - ro)
    where pos = pnt (worldX s x) (worldY s y) wz

traceScene:: Scene -> (Int, Int) -> Colour
traceScene s@(Scene shape light _ _ _ _) (x, y) = getColour s $ hit xs
    where ray_origin = pnt 0 0 (-5)
          r = getRay s (x,y)
          xs = intersectShapes shape r

getColour :: Scene -> Maybe Intersection -> Colour
getColour (Scene _ light _ _ _ _ ) (Just (Intersection t r@(Ray _ rd)  n m)) = lighting m light p (-rd) (n p)
    where p = pos r t
getColour _ Nothing                       = colour 0 0 0

-- material light position eye normal
lighting :: Material -> Light -> V4 Double -> V4 Double -> V4 Double -> Colour
lighting m@(Material mc ma md ms msh) l@(Light lp li) p e n = a + d + s
    where ecolour = mc * li
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
