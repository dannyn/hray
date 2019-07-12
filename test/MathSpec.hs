module MathSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Linear

import Math

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "math" $ do
    it "translation" $ do
        let v = vec (-3.0) 4.0 5.0
        let m = transMat $ vec 5 (-3) 2
        m !* v `shouldBe` (vec 2.0 1.0 7.0) 
    it "inverse translation" $ do
        let v = vec  2 1 7
        let m = inv44 . transMat $ vec 5 (-3) 2
        m !* v `shouldBe` (vec (-3.0) 4.0 5.0)
    it "scaling" $ do
        let v = vec (-4) 6 8
        let m = scaleMat $ vec 2 3 4
        m !* v `shouldBe` (vec (-8) 18 32)
    it "rotation x-axis" $ do
        let v = vec 0 1 0
        let m = rotXMat (pi/2)
        vecCmp (m!* v) (vec 0 0 1)
    it "rotation y-axis" $ do
        let v = vec 0 0 1
        let m = rotYMat (pi/2)
        vecCmp (m!* v) (vec 1 0 0)
    it "rotation z-axis" $ do
        let v = vec 0 1 0
        let m = rotZMat (pi/2)
        vecCmp (m!* v) (vec (-1) 0 0)
    it "multiple transformations" $ do
        let v = vec 1 0 1
        let rM = rotXMat (pi / 2)
        let sM = scaleMat (vec 5 5 5)
        let tM = transMat (vec 10 5 7)
        let m = tM !*! sM !*! rM
        vecCmp (m !* v) (vec 15 0 7)
