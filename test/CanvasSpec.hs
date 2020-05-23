module CanvasSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Canvas
import           Colour

main :: IO ()
main = hspec spec

black = colour 0 0 0
white = colour 255 255 255

line :: (Int, Int) -> Colour
line (x, y) | x == y = white
            | otherwise = black

spec :: Spec
spec = do
  describe "canvas" $ do
    it "create" $ do
        let a = [colour 0.0 0.0 0.0 | i <- [0..3]]
        canvas 2 2 `shouldBe` Canvas a 2 2
    it "width" $ do
        let c = canvas 5 3
        canvasWidth c `shouldBe` 5
    it "height" $ do
        let c = canvas 5 3
        canvasHeight c `shouldBe` 3
    it "header" $ do
        let c = canvas 3 5
        canvasHeader c `shouldBe` "P3\n3 5\n255\n"
    it "canvas to string" $ do
        let c1 = colour 1.0 1.0 1.0
        let c2 = colour 0.1 0.2 0.3
        let c3 = colour 1.0 0.0 0.0
        let c4 = colour 0.4 0.5 0.6
        let c = fromList 2 3 [c1, black, 
                              c2, black, 
                              black, c4]
        canvasToString c  `shouldBe` "255 255 255 0 0 0 25 51 76 0 0 0 0 0 0 102 127 153\n"
    it "wrapto70 wrap" $ do
        let s  = "1 2 3 4 1 a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 x n m p"
        let s' = "1 2 3 4 1 a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 x\n"
        let l = ["n", "m", "p"]
        wrapTo70 (words s) `shouldBe` (s', l)
    it "wrapto70 no wrap" $ do
        let s  = "a 1 1 1 1 1 1 1 1 1 1 1 b 1 1 1 1 1 c 1 1 1 1 1 1 1 1 1 1 1 1 1 1 d\n"
        wrapTo70 (words s) `shouldBe` (s, [])
    it "get wrapped lines" $ do
        let s  = words "1 2 3 4 1 a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 x n m p"
        let r  = "1 2 3 4 1 a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 x\nn m p\n"
        getWrappedLines s `shouldBe` r
    it "test functor" $ do
        let cc = coordCanvas 3 3
        let c = fromList 3 3 [ white, black, black
                             , black, white, black
                             , black, black, white ]
        fmap line cc `shouldBe` c
