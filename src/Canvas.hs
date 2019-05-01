module Canvas 
( Canvas(..)
, canvas
, getPixel
, setPixel
, canvasWidth
, canvasHeight
, canvasHeader
, canvasToString
) where

import Math.Vector
import Data.Array
import Text.Printf

-- The pixels in a Canvas are indexed as (x,y)
data Canvas = Canvas { pixels::Array Int (Array Int Color)} deriving (Show, Eq)

canvas :: Int -> Int -> Canvas
canvas x y = Canvas  (array(0,x-1) [ (n, row ) | n <- [0..x-1]])
    where black = color 0.0 0.0 0.0
          row   = array(0,y-1) [ (n, black) | n <- [0..y-1]]

getPixel :: Int -> Int -> Canvas -> Color
getPixel x y (Canvas p) =  (p ! x) ! y

setPixel :: Int -> Int -> Canvas -> Color -> Canvas
setPixel x y (Canvas p) c = Canvas ( p // [(x, row // [(y, c)])])
    where row = p ! x

canvasWidth :: Canvas -> Int
canvasWidth (Canvas p) = length $ indices p

canvasHeight :: Canvas -> Int
canvasHeight (Canvas p) = length $ p ! 0

canvasHeader :: Canvas -> String
canvasHeader c = printf "P3\n%d %d\n255" (canvasWidth c) (canvasHeight c)

canvasToString :: Canvas -> String
canvasToString (Canvas p) = tail $ foldl f "" (elems p)
    where f = \acc x -> acc ++ " " ++ (rowToString x)

rowToString :: (Array Int Color) -> String
rowToString a = tail $ foldl f "" (map colorToRGB (elems a))
    where f = \acc x -> acc ++ " " ++ x


-- saveToDisk:: Canvas -> IO ()
-- saveToDisk = do
