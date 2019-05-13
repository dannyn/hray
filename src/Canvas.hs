module Canvas 
( Canvas(..)
, canvas
, canvasPrintRow
, getPixel
, setPixel
, canvasWidth
, canvasHeight
, canvasHeader
, canvasToString
, canvasSaveToDisk
, wrapTo70
, getWrappedLines
, rowToString
) where

import System.IO
import Data.Array
import Text.Printf

import Math.Vector

-- The pixels in a Canvas are indexed as (x,y)
data Canvas = Canvas { pixels::Array Int (Array Int Color)} deriving (Show, Eq)

canvasPrintRow :: Canvas -> Int -> IO ()
canvasPrintRow (Canvas p) r = do
    print (p ! r)

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
canvasHeader c = printf "P3\n%d %d\n255\n" (canvasWidth c) (canvasHeight c)

canvasToString :: Canvas -> String
canvasToString (Canvas p) = getWrappedLines s
    where f = \acc x -> acc ++ " " ++ (rowToString x)
          s = tail $ foldl f "" (elems p)

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

rowToString :: (Array Int Color) -> String
rowToString a = tail $ foldl f "" (map colorToRGB (elems a))
    where f = \acc x -> acc ++ " " ++ x

canvasSaveToDisk:: Canvas -> IO ()
canvasSaveToDisk c = do
    writeFile "test.ppm" $ (canvasHeader c) ++ (canvasToString c)
    return ()
