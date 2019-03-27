module Math
( relEq
) where

-- For comparing Floats. 
epsilon = 0.0000000001

relEq :: Double -> Double -> Bool
relEq a b 
    | (abs (a - b)) <= epsilon = True
    | otherwise      = False
