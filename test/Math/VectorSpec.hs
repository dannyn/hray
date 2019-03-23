module Math.VectorSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Math.Vector 

main :: IO ()
main = hspec spec

instance Arbitrary Vector where
    arbitrary = do 
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        w <- arbitrary
        return (Vector x y z w) 

prop_Operations :: Vector -> Vector -> Bool
prop_Operations u v = u + v == v - (-u)

prop_Distributivity :: Vector -> Vector -> Double -> Bool
prop_Distributivity u v r = mulScalar (u+v) r == (mulScalar u r) + (mulScalar v r)

prop_Normalization :: Vector -> Bool
prop_Normalization v = norm v == 1.0

spec :: Spec
spec = do
  describe "math" $ do
    describe "vector" $ do
        it "create a point" $ do
            (point 4.3 (-4.2) 3.1) `shouldBe` (Vector 4.3 (-4.2) 3.1 1.0)
        it "create a vec" $ do
            (vec 4.3 (-4.2) 3.1) `shouldBe` (Vector 4.3 (-4.2) 3.1 0.0)
        it "addition" $ do
            let u = (Vector 3.0 (-2.0) 5.0 1.0)
            let v = (Vector (-2.0) 3.0 1.0 0.0)
            (u + v) `shouldBe` (Vector 1.0 1.0 6.0 1.0)
        it "subtraction" $ do
            let u = (point 3.0 2.0 1.0)
            let v = (point 5.0 6.0 7.0)
            (u - v) `shouldBe` (Vector (-2.0) (-4.0) (-6.0) 0.0)
        it "norm" $ do 
            let u = (vec 1.0 2.0 3.0)
            (norm u) `shouldBe` (sqrt 14.0)
        it "normalization" $ do
            let u = (vec 4.0 0.0 0.0)
            (normalize u) `shouldBe` Just (vec 1.0 0.0 0.0)
        it "dot" $ do
            let u = (vec 1.0 2.0 3.0)
            let v = (vec 2.0 3.0 4.0)
            (dot u v) `shouldBe` 20.0
        it "cross" $ do
            let u = (vec 1.0 2.0 3.0)
            let v = (vec 2.0 3.0 4.0)
            (cross u v) `shouldBe` (vec (-1.0) 2.0 (-1.0))
        it "operations" $ property $ prop_Operations
        it "distributivity" $ property $ prop_Distributivity
