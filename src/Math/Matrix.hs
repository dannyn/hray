{-# LANGUAGE TemplateHaskell #-}

module Math.Matrix
( Matrix(..)
, identity
, fromList
, getEntry
, byScaler 
) where

import Math
import Math.Vector

import qualified Data.Vector as V

data Matrix = Matrix (V.Vector Double) deriving (Show)

identity :: Matrix
identity = Matrix $ V.fromList [ 1.0, 0.0, 0.0, 0.0
                               , 0.0, 1.0, 0.0, 0.0
                               , 0.0, 0.0, 1.0, 0.0
                               , 0.0, 0.0, 0.0, 1.0 ]

fromList :: [Double] -> Matrix
fromList l = Matrix $ V.fromList l

translation :: Vector -> Matrix
translation (Vector x y z _) = fromList [ 1.0, 0.0, 0.0, x
                                              , 0.0, 1.0, 0.0, y
                                              , 0.0, 0.0, 1.0, z
                                              , 0.0, 0.0, 0.0, 1.0 ]

scaling :: Vector -> Matrix
scaling (Vector x y z _) = fromList [   x, 0.0, 0.0, 0.0
                                          , 0.0,   y, 0.0, 0.0
                                          , 0.0, 0.0,   z, 0.0
                                          , 0.0, 0.0, 0.0, 1.0 ]

getEntry :: Matrix -> Int -> Int -> Double
getEntry (Matrix m) x y = m V.!( (y * 4) + x)

instance Eq Matrix where
    (Matrix a) == (Matrix b)
        |  (relEq (a V.!0) (b V.!0)) && (relEq (a V.!1) (b V.!1)) && (relEq (a V.!2) (b V.!2)) && (relEq (a V.!3) (b V.!3)) 
        && (relEq (a V.!4) (b V.!4)) && (relEq (a V.!5) (b V.!5)) && (relEq (a V.!6) (b V.!6)) && (relEq (a V.!7) (b V.!7))
        && (relEq (a V.!8) (b V.!8)) && (relEq (a V.!9) (b V.!9)) && (relEq (a V.!10) (b V.!10)) && (relEq (a V.!11) (b V.!11)) 
        && (relEq (a V.!12) (b V.!12)) && (relEq (a V.!13) (b V.!13)) && (relEq (a V.!14) (b V.!14)) && (relEq (a V.!15) (b V.!15)) = True
        | otherwise = False

matrixMulEntry :: Matrix -> Matrix -> Int -> Int -> Double
matrixMulEntry m1 m2 x y = (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3)
    where a0 = getEntry m1 0 y
          a1 = getEntry m1 1 y
          a2 = getEntry m1 2 y
          a3 = getEntry m1 3 y 
          b0 = getEntry m2 x 0
          b1 = getEntry m2 x 1
          b2 = getEntry m2 x 2
          b3 = getEntry m2 x 3


instance Num Matrix where
    (Matrix a) + (Matrix b) = identity
    a * b = fromList [ matrixMulEntry a b 0 0
                           , matrixMulEntry a b 1 0 
                           , matrixMulEntry a b 2 0 
                           , matrixMulEntry a b 3 0 
                           , matrixMulEntry a b 0 1 
                           , matrixMulEntry a b 1 1 
                           , matrixMulEntry a b 2 1 
                           , matrixMulEntry a b 3 1 
                           , matrixMulEntry a b 0 2 
                           , matrixMulEntry a b 1 2 
                           , matrixMulEntry a b 2 2 
                           , matrixMulEntry a b 3 2 
                           , matrixMulEntry a b 0 3 
                           , matrixMulEntry a b 1 3 
                           , matrixMulEntry a b 2 3 
                           , matrixMulEntry a b 3 3 ] 
--    negate (Vector x y z w) = Vector (-x) (-y) (-z) (-w)
--    abs (Vector x y z w) = 1
--    fromInteger n = Vector (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n) 

byScaler :: Matrix -> Double -> Matrix
byScaler (Matrix m) d = Matrix (V.map (* d) m)

