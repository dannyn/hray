{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Canvas
import           Colour
import           Linear

red = colour 1.0 0.0 0.0
green = colour 0.0 1.0 0.0
blue = colour 0.0 0.0 1.0
white = colour 1.0 1.0 1.0

markCanvas :: Canvas Colour -> V3 Double -> Canvas Colour
markCanvas c (V3 vx vy _) = setPixel x y c red
    where x = floor vx
          y = canvasHeight c - floor vy

run :: Canvas Colour
run = canvas 100 100

main :: IO ()
main =  do
    canvasSaveToDisk run
