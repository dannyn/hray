{-# LANGUAGE TemplateHaskell #-}

module Math.Matrix
( Matrix(..)
) where

import Math.Vector

import qualified Data.Vector as V

data Matrix = Matrix (V.Vector Double)

identityMatrix :: Matrix
identityMatrix = Matrix $ V.fromList [ 1.0, 0.0, 0.0, 0.0
                 , 0.0, 1.0, 0.0, 0.0
                 , 0.0, 0.0, 1.0, 0.0
                 , 0.0, 0.0, 0.0, 1.0 ]

translation :: Vector -> Matrix
translation v = identityMatrix

scaling :: Vector -> Matrix
scaling v = identityMatrix

rotation :: Vector -> Matrix
rotation v = identityMatrix

--instance Num Vector where
--    (Vector a b c d) + (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
--    (Vector a b c d) * (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
--    negate (Vector x y z w) = Vector (-x) (-y) (-z) (-w)
--    abs (Vector x y z w) = 1
--    fromInteger n = Vector (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n) 

