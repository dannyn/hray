module Math (Vector(..), point, vec, norm, dot, cross, mulScalar )  where

-- For comparing Vectors. 
epsilon = 0.00000001

data Vector  = Vector { x :: Double
                      , y :: Double 
                      , z :: Double 
                      , w :: Double } deriving (Show)

instance Num Vector where
    (Vector a b c d) + (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
    (Vector a b c d) * (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
    negate (Vector x y z w) = Vector (-x) (-y) (-z) (-w)
    abs (Vector x y z w) = 1
    fromInteger n = Vector (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n) 

vec :: Double -> Double -> Double -> Vector
vec x y z = Vector x y z 0

point :: Double -> Double -> Double -> Vector
point x y z = Vector x y z 1

relEq :: Double -> Double -> Bool
relEq a b 
    | (abs (a - b)) <= epsilon = True
    | otherwise      = False

mulScalar :: Vector -> Double -> Vector
mulScalar (Vector x y z w) r = Vector (x*r) (y*r) (z*r) (w*r)

norm :: Vector -> Double
norm (Vector x y z _) = sqrt (x^2 + y^2 + z^2)

dot :: Vector -> Vector -> Double
dot (Vector a b c d) (Vector x y z w) = (a * x) + (b * y) + (c * z) + (d * w)

cross :: Vector -> Vector -> Vector
cross (Vector a b c d) (Vector x y z w) = vec (b*z - c*y)  (c*x -a*z) (a*y - b*x)

instance Eq Vector where
    (Vector a b c d) == (Vector x y z w) 
        | (relEq a x) && (relEq b y) && (relEq c z) && (relEq d w) = True
        | otherwise = False
