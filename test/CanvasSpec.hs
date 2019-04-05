module CanvasSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Canvas

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "canvas" $ do
    it "should be true" $ do
       True `shouldBe` True
