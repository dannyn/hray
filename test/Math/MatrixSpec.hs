module Math.MatrixSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Math
import Math.Vector 
import Math.Matrix

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "math" $ do
    describe "vector" $ do
        it "create a point" $ do
            True `shouldBe` True
