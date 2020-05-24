module Canvas
( Canvas(..)
, canvasWidth
, canvasHeight
, canvasHeader
, coordCanvas
, canvas
, canvasToString
, canvasSaveToDisk
, fromList
, wrapTo70
, addSpaces
, getWrappedLines
, pmap
) where

import           System.IO
import           Text.Printf
import           Control.Parallel.Strategies

import           Colour

data Canvas a = Canvas  { pixels :: [a]
                        , width  :: Int
                        , height :: Int } deriving (Show, Eq)

instance Functor Canvas where
    fmap f (Canvas p w h) = Canvas c w h
        where c = fmap f p

pmap f (Canvas p w h) = Canvas c w h 
    where c = fmap f p `using` parListChunk 5000 rseq

-- We transform a canvas of coordinates to a canvas of colours
-- f :: Sphere -> Canvas (Int, Int) -> Canvas Colour
-- f is expected to be curried in order to pass it information about the world such
-- as lights and models.
--

canvasWidth :: Canvas a -> Int
canvasWidth (Canvas _ w _ ) = w

canvasHeight :: Canvas a -> Int
canvasHeight (Canvas _ _ h ) = h

canvasHeader :: Canvas a -> String
canvasHeader (Canvas _ w h) = printf "P3\n%d %d\n255\n" w h

coordCanvas :: Int -> Int -> Canvas (Int, Int)
coordCanvas w h = Canvas [(x,y) | x <- [0..w-1], y <- [0..h-1]] w h

canvas :: Int -> Int -> Canvas Colour
canvas w h = Canvas pixels w h
    where black  = colour 0.0 0.0 0.0
          pixels = [black | n <- [1..w*h]]

fromList :: Int -> Int -> [a] -> Canvas a
fromList w h xs = Canvas xs w h

canvasToString :: Canvas Colour -> String
canvasToString (Canvas p _ _) = getWrappedLines s
    where f acc x = acc ++ x
          s = concat (map colourToRGB p)

canvasSaveToDisk:: String -> Canvas Colour -> IO ()
canvasSaveToDisk fn c = do
    writeFile fn $ canvasHeader c ++ canvasToString c
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
