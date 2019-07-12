module Math
( V3(..)
, M44(..)
, vec
, vecCmp
, idVec
, transMat
, rotMat
, scaleMat
, rotXMat
, rotYMat
, rotZMat
) where

import Linear

vec :: Double -> Double -> Double -> V4 Double
vec x y z = V4 x y z 1.0

vecCmp :: V4 Double -> V4 Double -> Bool
vecCmp u v= nearZero (u - v) 

idVec :: V4 Double
idVec = vec 0 0 0

transMat :: V4 Double -> M44 Double
transMat (V4 x y z _) = mkTransformationMat identity (V3 x y z)

rotMat :: V3 Double -> V3 Double -> V3 Double -> M44 Double
rotMat vx vy vz = identity :: M44 Double

scaleMat :: V4 Double -> M44 Double
scaleMat = scaled

rotXMat :: Double -> M44 Double
rotXMat r = mkTransformation q (V3 0 0 0)
    where q = axisAngle (V3 1 0 0) r

rotYMat :: Double -> M44 Double
rotYMat r = mkTransformation q (V3 0 0 0)
    where q = axisAngle (V3 0 1 0) r

rotZMat :: Double -> M44 Double
rotZMat r = mkTransformation q (V3 0 0 0)
    where q = axisAngle (V3 0 0 1) r
