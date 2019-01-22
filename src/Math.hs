module Math (Vector(..), point, vec )  where

data Vector  = Vector { x :: Double
                      , y :: Double 
                      , z :: Double 
                      , w :: Double } deriving (Show)

instance Num Vector where
    (Vector a b c d) + (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
    (Vector a b c d) * (Vector x y z w) = Vector (a+x) (b+y) (c+z) (d+w)
    negate (Vector x y z w) = Vector (-x) (-y) (-z) (-w)
    abs (Vector x y z w) =  1
    fromInteger n = Vector (fromIntegral n) (fromIntegral n) (fromIntegral n) (fromIntegral n) 

vec x y z = Vector x y z 0
point x y z = Vector x y z 1

epsilon = 0.00000001

relEq a b 
    | (abs (a - b)) <= epsilon = True
    | otherwise      = False

instance Eq Vector where
    (Vector a b c d) == (Vector x y z w) 
        | (relEq a x) && (relEq b y) && (relEq c z) && (relEq d w) = True
        | otherwise = False
