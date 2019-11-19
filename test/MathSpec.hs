module MathSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Linear

import           Math

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "math" $ do
    it "translation" $ do
        let v = pnt (-3.0) 4.0 5.0
        let m = transMat $ pnt 5 (-3) 2
        m !* v `shouldBe` (pnt 2.0 1.0 7.0)
    it "inverse translation" $ do
        let v = pnt  2 1 7
        let m = inv44 . transMat $ pnt 5 (-3) 2
        m !* v `shouldBe` (pnt (-3.0) 4.0 5.0)
    it "scaling" $ do
        let v = pnt (-4) 6 8
        let m = scaleMat $ pnt 2 3 4
        m !* v `shouldBe` (pnt (-8) 18 32)
    it "rotation x-axis" $ do
        let v = pnt 0 1 0
        let m = rotXMat (pi/2)
        vecCmp (m!* v) (pnt 0 0 1)
    it "rotation y-axis" $ do
        let v = pnt 0 0 1
        let m = rotYMat (pi/2)
        vecCmp (m!* v) (pnt 1 0 0)
    it "rotation z-axis" $ do
        let v = pnt 0 1 0
        let m = rotZMat (pi/2)
        vecCmp (m!* v) (pnt (-1) 0 0)
    it "multiple transformations" $ do
        let v = pnt 1 0 1
        let rM = rotXMat (pi / 2)
        let sM = scaleMat (pnt 5 5 5)
        let tM = transMat (pnt 10 5 7)
        let m = tM !*! sM !*! rM
        vecCmp (m !* v) (pnt 15 0 7)
