module Canvas 
( Canvas(..)
, canvas
, getPixel
, setPixel
, canvasWidth
, canvasHeight
, canvasHeader
, canvasToString
, canvasSaveToDisk
, wrapTo70
, getWrappedLines
) where

import System.IO
import Data.Array
import Text.Printf

import Math.Vector

-- The pixels in a Canvas are indexed as (x,y)
data Canvas = Canvas { pixels :: Array Int Color
                     , width :: Int 
                     , height :: Int } deriving (Show, Eq)

canvas :: Int -> Int -> Canvas
canvas w h = Canvas (array(0, (w * h) -1) [ (n, black) | n <- [0..(w * h) -1]]) w h
    where black = color 0.0 0.0 0.0

getPixel :: Int -> Int -> Canvas -> Color
getPixel x y (Canvas p w h) =  p ! ( (y * w) + x)

setPixel :: Int -> Int -> Canvas -> Color -> Canvas
setPixel x y (Canvas p w h) c = Canvas ( p // [(i, c)] ) w h
    where i = ( (y * w) + x)

canvasWidth :: Canvas -> Int
canvasWidth (Canvas _ w _ ) = w

canvasHeight :: Canvas -> Int
canvasHeight (Canvas _ _ h ) = h

canvasHeader :: Canvas -> String
canvasHeader (Canvas _ w h) = printf "P3\n%d %d\n255\n" w h

canvasToString :: Canvas -> String
canvasToString (Canvas p _ _) = getWrappedLines s
    where f = \acc x -> acc ++ " " ++ x
          s = tail $ foldl f "" (map colorToRGB (elems p))

getWrappedLines :: String -> String
getWrappedLines s = (getWrappedLines' (words s) "")

getWrappedLines' :: [String] -> String -> String
getWrappedLines' [] s = s 
getWrappedLines' xs s = getWrappedLines' nxs (s ++ ns ++ "\n")
    where (ns, nxs) = wrapTo70 xs

wrapTo70 :: [String] -> (String, [String])
wrapTo70 xs = wrapTo70' xs ""

wrapTo70' :: [String] -> String -> (String, [String])
wrapTo70' (x:xs) acc 
        | len > 70  = (tail acc, [x] ++ xs)
        | xs==[]    = (tail newAcc, [])
        | otherwise = wrapTo70' xs newAcc
    where newAcc = acc ++ " " ++ x
          len = length newAcc

canvasSaveToDisk:: Canvas -> IO ()
canvasSaveToDisk c = do
    writeFile "test.ppm" $ (canvasHeader c) ++ (canvasToString c)
    return ()
