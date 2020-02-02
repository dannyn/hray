module Ray
(
  Ray(..)
, transformRay
, pos
) where

import           Data.List

import           Colour
import           Linear
import           Math

data Ray = Ray { origin    :: V4 Double
               , direction :: V4 Double } deriving (Show)

instance Eq Ray where
    (==) (Ray o1 d1) (Ray o2 d2) = vecCmp o1 o2 && vecCmp d1 d2

transformRay :: Ray -> M44 Double -> Ray
transformRay (Ray o d) m = Ray (m !* o) ( m !* d)

pos :: Ray -> Double -> V4 Double
pos (Ray o d) t = o + (d ^* t)
