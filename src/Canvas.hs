module Canvas
( Canvas(..)
, canvasWidth
, canvasHeight
, canvasHeader
, canvas
, getPixel
, setPixel
, canvasToString
, canvasSaveToDisk
, wrapTo70
, addSpaces
, getWrappedLines
) where

import           System.IO
import           Text.Printf

import qualified Data.Vector as V

import           Colour

data Canvas a = Canvas  { pixels :: V.Vector a
                     , width     :: Int
                     , height    :: Int } deriving (Show, Eq)

instance Functor Canvas where
    fmap f (Canvas p w h) = Canvas (fmap f p) w h

--trace :: Objects -> Lights -> Camera -> Canvas (x,y) -> Canvas Colour

canvasWidth :: Canvas a -> Int
canvasWidth (Canvas _ w _ ) = w

canvasHeight :: Canvas a -> Int
canvasHeight (Canvas _ _ h ) = h

canvasHeader :: Canvas a -> String
canvasHeader (Canvas _ w h) = printf "P3\n%d %d\n255\n" w h

canvas :: Int -> Int -> Canvas Colour
canvas w h = Canvas pixels w h
    where black  = colour 0.0 0.0 0.0
          pixels = V.fromList [black | n <- [1..w*h]]

getPixel :: Int -> Int -> Canvas Colour -> Colour
getPixel x y (Canvas p w h) =  p V.! ( (y * w) + x)

setPixel :: Int -> Int -> Canvas Colour -> Colour -> Canvas Colour
setPixel x y (Canvas p w h) c = Canvas ( p V.//  [(i,c)] )  w h
    where i = (y * w) + x

canvasToString :: Canvas Colour -> String
canvasToString (Canvas p _ _) = getWrappedLines s
    where f = \acc x -> acc ++ x
          s = concat (V.map colourToRGB p)

canvasSaveToDisk:: Canvas Colour -> IO ()
canvasSaveToDisk c = do
    writeFile "test.ppm" $ canvasHeader c ++ canvasToString c
    return ()

getWrappedLines :: [String] -> String
getWrappedLines s = concat  (getWrappedLines' s )

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
addSpaces []     = []
addSpaces [x]    = x : ["\n"]
addSpaces (x:xs) = x : " " : addSpaces xs

wrapTo70' :: [String] -> [String] -> Int -> ([String], [String])
wrapTo70' (x:xs) acc l
        | l >= (69 - length x)  = ([x], xs)
        | otherwise = (x : ns, nxs)
    where (ns, nxs) = wrapTo70' xs acc (l + 1 + length x)
wrapTo70' [] acc l = ( [], [])
