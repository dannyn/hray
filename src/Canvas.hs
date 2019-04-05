module Canvas () where

import Math.Vector
import Data.Array

-- The pixels in a Canvas are indexed as (x,y)
data Canvas = Canvas { pixels::Array Int (Array Int Color)}

canvas :: Int -> Int -> Canvas
canvas x y = Canvas  (array(0,x-1) [ (n, row ) | n <- [0..x-1]])
    where black = color 0.0 0.0 0.0
          row   = array(0,y-1) [ (n, black) | n <- [0..y-1]]

getPixel :: Int -> Int -> Canvas -> Color
getPixel x y (Canvas p) =  (p ! x) ! y

setPixel :: Int -> Int -> Canvas -> Color -> Canvas
setPixel x y (Canvas p) c = Canvas ( p // [(x, row // [(y, c)])])
    where row = p ! x

width c = 2
height c = 2
