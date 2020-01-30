module Main where

import           Options.Applicative
import           Data.Semigroup ((<>))

import           Canvas
import           Colour
import           Linear
import           Scene

data Configuration = Configuration 
    { filename :: String }

parseArgs :: Parser Configuration
parseArgs = Configuration
    <$> strOption
        ( long "filename" )

run :: Configuration -> IO ()
run (Configuration fn) = canvasSaveToDisk fn $ fmap (traceScene scene) (coordCanvas 100 100)

main :: IO ()
main = run =<< execParser opts
    where opts = info (parseArgs <**> helper) 
                      ( fullDesc <> header "hray - a ray tracer in haskell" )
