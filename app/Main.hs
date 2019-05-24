{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

import Math.Vector
import Canvas

red = color 1.0 0.0 0.0
green = color 0.0 1.0 0.0
blue = color 0.0 0.0 1.0
white = color 1.0 1.0 1.0

data Projectile = Projectile { _pPos ::Vector 
                             , _pVelocity :: Vector } deriving (Show)
makeLenses ''Projectile

data Environment = Environment { _eCanvas :: Canvas 
                               , _eProj   :: Projectile 
                               , _eGrav   :: Vector 
                               , _eWind   :: Vector } deriving (Show)
makeLenses ''Environment

environment :: Vector -> Vector -> Environment
environment grav wind = Environment (canvas 500 500) p grav wind
    where p = Projectile (point 0.0 1.0 0.0) (vec 1.15 1.0 0.0)

tickProjectile :: Projectile -> Vector -> Vector -> Projectile
tickProjectile (Projectile pos vel) grav wind = Projectile (pos + vel) (vel + grav + wind)

markCanvas :: Canvas -> Vector -> Canvas
markCanvas c v = setPixel x y c red
    where x = floor (v^.vX)
          y = (canvasHeight c) - floor (v^.vY)

tickEnvironment :: Environment -> Environment
tickEnvironment e = Environment canv proj (e^.eGrav) (e^.eWind)
    where proj = tickProjectile (e^.eProj) (e^.eGrav) (e^.eWind)
          canv = markCanvas (e^.eCanvas) (e^.eProj.pPos)

isFinished :: Projectile -> Bool
isFinished (Projectile (Vector _ y _ _ ) _) | y > 0 = False
                                            | otherwise = True

run :: Environment -> Canvas
run e | isFinished (e ^. eProj) = e^.eCanvas
      | otherwise = run $ tickEnvironment e

main :: IO ()
main =  do 
    canvasSaveToDisk (run $ environment (vec 0.0 (-0.08) 0.0) (vec (0.03) 0.0 0.0))
