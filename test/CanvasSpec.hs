module CanvasSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Array

import Canvas
import Math.Vector

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "canvas" $ do
    it "create" $ do
        let a = array (0,3) [(i, (color 0.0 0.0 0.0)) | i <- [0..3]]
        (canvas 2 2) `shouldBe` Canvas a 2 2
    it "set and get" $ do 
        let c = setPixel 5 3 (canvas 6 4) (color 1.0 1.0 1.0)
        (color 1.0 1.0 1.0) `shouldBe` getPixel 5 3 c
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
        let c1 = setPixel 0 0 (canvas 2 2) (color 1.0 1.0 1.0)
        let c2 = setPixel 0 1 c1 (color 0.0 1.0 0.0)
        let c3 = setPixel 1 1 c2 (color 1.0 0.0 0.0)
        canvasToString c3 `shouldBe` "255 255 255 0 0 0 0 255 0 255 0 0\n"
    it "wrapto70 wrap" $ do
        let s  = "1 2 3 4 1 a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 x n m p" 
        let s' = "1 2 3 4 1 a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 x"
        let l = ["n", "m", "p"]
        wrapTo70 (words s) `shouldBe` (s', l)
    it "wrapto70 no wrap" $ do
        let s  = "a 1 1 1 1 1 1 1 1 1 1 1 b 1 1 1 1 1 c 1 1 1 1 1 1 1 1 1 1 1 1 1 1 d" 
        wrapTo70 (words s) `shouldBe` (s, [])
    it "get wrapped lines" $ do
        let s  = "1 2 3 4 1 a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 x n m p" 
        let r  = "1 2 3 4 1 a 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 x\nn m p\n" 
        getWrappedLines s `shouldBe` r
