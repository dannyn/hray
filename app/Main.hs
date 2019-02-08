module Main where

import Math

v = Vector 1.0 2.0 3.0 4.0
u = Vector 2.0 3.0 4.0 5.0

data Projectile = Projectile Vector Vector deriving (Show)
data Environment = Environment Vector Vector

tick :: Environment -> Projectile -> Projectile
tick (Environment grav wind) (Projectile pos vel) = Projectile (pos + vel) (vel + grav + wind)

run :: Environment -> Projectile -> IO ()
run e p = do
    let new_p = tick e p
    print new_p
    run e new_p

main :: IO ()
main =  run e p
    where
        e = Environment (vec 0.0 (-0.1) 0.0) (vec (-0.01) 0.0 0.0)
        p = Projectile (point 0.0 1.0 0.0) (vec 1.0 1.0 0.0)

