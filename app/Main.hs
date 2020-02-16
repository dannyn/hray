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

scene = Scene (sphere unitSphere) (pnt 0 0 (-5)) 500 7 10

run :: Configuration -> IO ()
run (Configuration fn) = canvasSaveToDisk fn canvas
    where canvas = fmap (traceScene scene) (coordCanvas 500 500)

main :: IO ()
main = run =<< execParser opts
    where opts = info (parseArgs <**> helper)
                      ( fullDesc <> header "hray - a ray tracer in haskell" )
