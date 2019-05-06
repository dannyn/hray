{-# LANGUAGE TemplateHaskell #-}

module Math.Vector 
( Vector(..)
, Color(..)
, color
, point
, vec
, norm
, normalize
, dot
, cross
, mulScalar 
, hadamard
, colorToRGB
, vX
, vY
, vZ
) where

import Control.Lens

import Math


data Vector  = Vector { _vX :: Double
                      , _vY :: Double 
                      , _vZ :: Double 
                      , _vW :: Double } deriving (Show)

makeLenses ''Vector

type Color = Vector

instance Num Vector where
    (Vector a b c d) + (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
    (Vector a b c d) * (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
    negate (Vector x y z w) = Vector (-x) (-y) (-z) (-w)
    abs (Vector x y z w) = 1
    fromInteger n = Vector (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n) 

color :: Double -> Double -> Double -> Color
color r g b = vec r g b

vec :: Double -> Double -> Double -> Vector
vec x y z = Vector x y z 0

point :: Double -> Double -> Double -> Vector
point x y z = Vector x y z 1

mulScalar :: Vector -> Double -> Vector
mulScalar (Vector x y z w) r = Vector (x*r) (y*r) (z*r) (w*r)

norm :: Vector -> Double
norm (Vector x y z _) = sqrt (x^2 + y^2 + z^2)

normalize :: Vector -> Maybe Vector
normalize (Vector x y z w)
        | n > 0     = Just (Vector (x / n) (y / n) (z / n) w)
        | otherwise = Nothing
        where n = norm (Vector x y z w)

dot :: Vector -> Vector -> Double
dot (Vector a b c d) (Vector x y z w) = (a * x) + (b * y) + (c * z) + (d * w)

cross :: Vector -> Vector -> Vector
cross (Vector a b c d) (Vector x y z w) = vec (b*z - c*y)  (c*x -a*z) (a*y - b*x)

instance Eq Vector where
    (Vector a b c d) == (Vector x y z w) 
        | (relEq a x) && (relEq b y) && (relEq c z) && (relEq d w) = True
        | otherwise = False

hadamard :: Color -> Color -> Color
hadamard (Vector r1 g1 b1 _) (Vector r2 g2 b2 _) = Vector (r1*r2) (g1*g2) (b1*b2) 0.0

colorToRGB :: Color -> String
colorToRGB (Vector r g b _) = (rgb r) ++ " " ++ (rgb g) ++ " " ++ (rgb b)
    where rgb = \d -> show . floor $ d * 255
