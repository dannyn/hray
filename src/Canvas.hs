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
, addSpaces
, getWrappedLines
) where

import System.IO
import Text.Printf

import qualified Data.Vector as V

import Math.Vector

data Canvas = Canvas { pixels :: V.Vector Color
                     , width :: Int 
                     , height :: Int } deriving (Show, Eq)

canvas :: Int -> Int -> Canvas
canvas w h = Canvas pixels w h
    where black  = color 0.0 0.0 0.0
          pixels = V.fromList [black | n <- [1..w*h]] 

getPixel :: Int -> Int -> Canvas -> Color
getPixel x y (Canvas p w h) =  p V.! ( (y * w) + x)

setPixel :: Int -> Int -> Canvas -> Color -> Canvas
setPixel x y (Canvas p w h) c = Canvas ( p V.//  [(i,c)] )  w h
    where i = ( (y * w) + x)

canvasWidth :: Canvas -> Int
canvasWidth (Canvas _ w _ ) = w

canvasHeight :: Canvas -> Int
canvasHeight (Canvas _ _ h ) = h

canvasHeader :: Canvas -> String
canvasHeader (Canvas _ w h) = printf "P3\n%d %d\n255\n" w h

-- 
--  Take each color as a string on its own, and operate on these instead of the individual components.
-- 
canvasToString :: Canvas -> String
canvasToString (Canvas p _ _) = getWrappedLines s
    where f = \acc x -> acc ++ x
          s = concat (V.map colorToRGB p)

getWrappedLines :: [String] -> String
getWrappedLines s = concat . getWrappedLines' $ s 

getWrappedLines' :: [String] -> [String]
getWrappedLines' []  = []
getWrappedLines' xs = ns : getWrappedLines' nxs
    where (ns, nxs) = wrapTo70 xs

-- Takes a list of strings and returns a string made from the first
-- several of length no more than 70 along with the remainder of 
-- the list.
wrapTo70 :: [String] -> (String, [String])
wrapTo70 xs = (concat . addSpaces $ ns, nxs)
    where (ns, nxs) = wrapTo70' xs [] 1

-- Add a space between every string in the list, and add a newline at the end.
addSpaces :: [String] -> [String]
addSpaces [] = []
addSpaces [x] = x : ["\n"]
addSpaces (x:xs) = x : " " : addSpaces xs

wrapTo70' :: [String] -> [String] -> Int -> ([String], [String])
wrapTo70' (x:xs) acc l
        | l >= (69 - length x)  = ([x], xs)
        | otherwise = (x : ns, nxs)
    where (ns, nxs) = wrapTo70' xs acc (l+1 + (length x)) 
wrapTo70' [] acc l = ( [], [])

canvasSaveToDisk:: Canvas -> IO ()
canvasSaveToDisk c = do
    writeFile "test.ppm" $ (canvasHeader c) ++ (canvasToString c)
    return ()
