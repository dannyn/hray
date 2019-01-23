module Math.MathSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Math

main :: IO ()
main = hspec spec

instance Arbitrary Vector where -- => Arbitrary (Double) where
    arbitrary = do 
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        w <- arbitrary
        return (Vector x y z w) 
        

prop_Operations :: Vector -> Vector -> Bool
prop_Operations u v = u + v == v - (-u)
 
spec :: Spec
spec = do
  describe "math" $ do
    it "create a point" $ do
        (point 4.3 (-4.2) 3.1) `shouldBe` (Vector 4.3 (-4.2) 3.1 1.0)
    it "create a vec" $ do
        (vec 4.3 (-4.2) 3.1) `shouldBe` (Vector 4.3 (-4.2) 3.1 0.0)
    it "subtraction" $ do
        let u = (point 3.0 2.0 1.0)
        let v = (point 5.0 6.0 7.0)
        (u - v) `shouldBe` (Vector (-2.0) (-4.0) (-6.0) 0.0)
    it "addition" $ do
        let u = (Vector 3.0 (-2.0) 5.0 1.0)
        let v = (Vector (-2.0) 3.0 1.0 0.0)
        (u + v) `shouldBe` (Vector 1.0 1.0 6.0 1.0)
    it "operations" $ property $ prop_Operations
