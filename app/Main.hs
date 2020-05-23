module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Canvas
import           Colour
import           Linear
import           Scene
import           Math
import           Sphere

data Configuration = Configuration
    { filename :: String }

parseArgs :: Parser Configuration
parseArgs = Configuration
    <$> strOption
        ( long "filename"
       <> short 'f'
       <> showDefault
       <> value "test.ppm" )

l = Light (pnt (-10) 10 (-10)) (colour 1 1 1)
scene = Scene ([sphere unitSphere]) l (pnt 0 0 (-5)) 1000 7 10

run :: Configuration -> IO ()
run (Configuration fn) = canvasSaveToDisk fn canvas
    where canvas = fmap (traceScene scene) (coordCanvas 1000 1000)

main :: IO ()
main = run =<< execParser opts
    where opts = info (parseArgs <**> helper)
                      ( fullDesc <> header "hray - a ray tracer in haskell" )
