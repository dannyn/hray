module Math.MathSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Math

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "math" $ do
    it "create a point" $ do
        (point 4.3 (-4.2) 3.1) `shouldBe` (Vector 4.3 (-4.2) 3.1 1.0)
    it "create a vec" $ do
        (vec 4.3 (-4.2) 3.1) `shouldBe` (Vector 4.3 (-4.2) 3.1 0.0)
    it "addition" $ do
        (u + v) `shouldBe` (Vector 1.0 1.0 6.0 1.0)
        where
            u = (Vector 3.0 (-2.0) 5.0 1.0)
            v = (Vector (-2.0) 3.0 1.0 0.0)
