module SceneSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Math
import           Ray
import           Scene

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sphere" $ do
    it "intersect at tangent" $ do
        let r = Ray (pnt 0 1 (-5)) (vec 0 0 1)
        intersects unitSphere r `shouldBe` [5.0, 5.0]
    it "misses" $ do
        let r = Ray (pnt 0 2 (-5)) (vec 0 0 1)
        intersects unitSphere r `shouldBe` []
    it "starts inside" $ do
        let r = Ray (pnt 0 0 0) (vec 0 0 1)
        intersects unitSphere  r `shouldBe` [-1.0, 1.0]
    it "behind" $ do
        let r = Ray (pnt 0 0 5) (vec 0 0 1)
        intersects unitSphere r `shouldBe` [-6.0, -4.0]
    it "scaled intersection" $ do
        let r = Ray (pnt 0 0 (-5)) (vec 0 0 1)
        let m = scaleMat (pnt 2 2 2)
        let s = Sphere 1 m
        intersects s r `shouldBe` [3, 7]
    it "scaled intersection" $ do
        let r = Ray (pnt 0 0 (-5)) (vec 0 0 1)
        let m = transMat (pnt 5 0 0)
        let s = Sphere 1 m
        intersects s r `shouldBe` []
