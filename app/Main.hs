module Main where

import Math.Vector
import Canvas

v = Vector 1.0 2.0 3.0 4.0
u = Vector 2.0 3.0 4.0 5.0

data Projectile = Projectile Vector Vector deriving (Show)
data Environment = Environment Vector Vector

tick :: Environment -> Projectile -> Projectile
tick (Environment grav wind) (Projectile pos vel) = Projectile (pos + vel) (vel + grav + wind)

isFinished :: Projectile -> Bool
isFinished (Projectile (Vector _ y _ _ ) _) | y > 0 = False
                                            | otherwise = True

run :: Environment -> Projectile -> IO ()
run e p = do
    if isFinished new_p
        then
            print new_p
        else
            run e new_p
    where
        new_p = tick e p

red = color 1.0 0.0 0.0
green = color 0.0 1.0 0.0
blue = color 0.0 0.0 1.0
white = color 1.0 1.0 1.0

main :: IO ()
main =  do 
    canvasSaveToDisk c3
    canvasPrintRow c3 0
    canvasPrintRow c3 1
    where
        e = Environment (vec 0.0 (-0.1) 0.0) (vec (-0.01) 0.0 0.0)
        p = Projectile (point 0.0 1.0 0.0) (vec 1.0 1.0 0.0)
        --c1 = setPixel 4 0 (canvas 5 3) (color 1.0 1.0 1.0)
        --c = setPixel 1 2 c1 (color 0.0 1.0 0.0)
        c1 = setPixel 0 0 (canvas 3 4) white
        c2 = setPixel 0 1 c1 green
        c3 = setPixel 1 1 c2 red


