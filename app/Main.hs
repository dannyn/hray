{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

import Canvas
import Colour
import Linear

red = colour 1.0 0.0 0.0
green = colour 0.0 1.0 0.0
blue = colour 0.0 0.0 1.0
white = colour 1.0 1.0 1.0

data Projectile = Projectile { _pPos ::V3 Double
                             , _pVelocity :: V3 Double} deriving (Show)
makeLenses ''Projectile

data Environment = Environment { _eCanvas :: Canvas 
                               , _eProj   :: Projectile 
                               , _eGrav   :: V3 Double
                               , _eWind   :: V3 Double} deriving (Show)
makeLenses ''Environment

environment :: V3 Double -> V3 Double -> Environment
environment grav wind = Environment (canvas 100 100) p grav wind
    where p = Projectile (V3 0.0 1.0 0.0) (V3 1.15 1.0 0.0)

tickProjectile :: Projectile -> V3 Double -> V3 Double -> Projectile
tickProjectile (Projectile pos vel) grav wind = Projectile (pos + vel) (vel + grav + wind)

markCanvas :: Canvas -> V3 Double -> Canvas
markCanvas c (V3 vx vy _) = setPixel x y c red
    where x = floor vx
          y = (canvasHeight c) - floor vy

tickEnvironment :: Environment -> Environment
tickEnvironment e = Environment canv proj (e^.eGrav) (e^.eWind)
    where proj = tickProjectile (e^.eProj) (e^.eGrav) (e^.eWind)
          canv = markCanvas (e^.eCanvas) (e^.eProj.pPos)

isFinished :: Projectile -> Bool
isFinished (Projectile (V3 _ y _ ) _) | y > 0 = False
                                            | otherwise = True

run :: Environment -> Canvas
run e | isFinished (e ^. eProj) = e^.eCanvas
      | otherwise = run $ tickEnvironment e

main :: IO ()
main =  do 
    canvasSaveToDisk (run $ environment (V3 0.0 (-0.08) 0.0) (V3 (0.03) 0.0 0.0))
