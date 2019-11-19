module RaySpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Math
import           Ray

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ray" $ do
    it "position" $ do
        let r = Ray (pnt 2 3 4) (vec 1 0 0)
        pos r (-1) `shouldBe` pnt 1 3 4
    it "translate" $ do
        let r = Ray (pnt 1 2 3) (vec 0 1 0)
        let m = transMat (pnt 3 4 5)
        transformRay r m `shouldBe` Ray (pnt 4 6 8) (vec 0 1 0)
    it "scale" $ do
        let r = Ray (pnt 1 2 3) (vec 0 1 0)
        let m = scaleMat (pnt 2 3 4)
        transformRay r m `shouldBe` Ray (pnt 2 6 12) (vec 0 3 0)
  describe "sphere" $ do
    it "intersect at tangent" $ do
        let r = Ray (pnt 0 1 (-5)) (vec 0 0 1)
        intersects sphere r `shouldBe` [5.0, 5.0]
    it "misses" $ do
        let r = Ray (pnt 0 2 (-5)) (vec 0 0 1)
        intersects sphere r `shouldBe` []
    it "starts inside" $ do
        let r = Ray (pnt 0 0 0) (vec 0 0 1)
        intersects sphere  r `shouldBe` [-1.0, 1.0]
    it "behind" $ do
        let r = Ray (pnt 0 0 5) (vec 0 0 1)
        intersects sphere r `shouldBe` [-6.0, -4.0]
    it "hit, all positive" $ do
        let i1 = Intersection 1 0
        let i2 = Intersection 2 0
        let xs = sortIntersections [i1, i2]
        hit xs `shouldBe` Just i1
    it "hit, some negative" $ do
        let i1 = Intersection (-1) 0
        let i2 = Intersection 2 0
        let xs = sortIntersections [i1, i2]
        hit xs `shouldBe` Just i2
    it "hit, all negative" $ do
        let i1 = Intersection (-2) 0
        let i2 = Intersection (-1) 0
        let xs = sortIntersections [i1, i2]
        hit xs `shouldBe` Nothing
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
