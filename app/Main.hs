module Main where

import           Canvas
import           Colour
import           Linear
import           Scene

run :: Canvas Colour
run = fmap (traceScene scene) (coordCanvas 100 100)

main :: IO ()
main = canvasSaveToDisk run
