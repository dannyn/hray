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
        let a = array (0,1) [(i, (color 0.0 0.0 0.0)) | i <- [0..1]]
        let c = array (0,1) [(i, a) | i <- [0..1]]
        (canvas 2 2) `shouldBe` Canvas c
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
        canvasHeader c `shouldBe` "P3\n3 5\n255"
    it "canvas to string" $ do 
        let c = setPixel 0 0 (canvas 2 1) (color 1.0 1.0 1.0)
        canvasToString c `shouldBe` "255 255 255 0 0 0"
