module Math
( V3(..)
, M44(..)
, vec
, pnt
, vecCmp
, dblCmp
, idVec
, zeroW
, reflect
, transMat
, rotMat
, scaleMat
, rotXMat
, rotYMat
, rotZMat
) where

import           Linear

vec :: Double -> Double -> Double -> V4 Double
vec x y z = V4 x y z 0.0

pnt :: Double -> Double -> Double -> V4 Double
pnt x y z = V4 x y z 1.0

vecCmp :: V4 Double -> V4 Double -> Bool
vecCmp u v= nearZero (u - v)

dblCmp :: Double -> Double -> Bool
dblCmp x y = nearZero (x - y)

idVec :: V4 Double
idVec = vec 0 0 0

zeroW :: V4 Double -> V4 Double
zeroW (V4 x y z _) = V4 x y z 0.0

reflect :: V4 Double -> V4 Double -> V4 Double
reflect v n = v - n ^* (2 * d)
    where d = dot v n

-- Always pass a pnt
transMat :: V4 Double -> M44 Double
transMat (V4 x y z _) = mkTransformationMat identity (V3 x y z)

-- Always pass a pnt
rotMat :: V3 Double -> V3 Double -> V3 Double -> M44 Double
rotMat vx vy vz = identity :: M44 Double

-- Always pass a pnt
scaleMat :: V4 Double -> M44 Double
scaleMat = scaled

-- Always pass a pnt
rotXMat :: Double -> M44 Double
rotXMat r = mkTransformation q (V3 0 0 0)
    where q = axisAngle (V3 1 0 0) r

-- Always pass a pnt
rotYMat :: Double -> M44 Double
rotYMat r = mkTransformation q (V3 0 0 0)
    where q = axisAngle (V3 0 1 0) r

-- Always pass a pnt
rotZMat :: Double -> M44 Double
rotZMat r = mkTransformation q (V3 0 0 0)
    where q = axisAngle (V3 0 0 1) r
