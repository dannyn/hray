module SceneSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Math
import           Ray
import           Scene

main :: IO ()
main = hspec spec

s = sphere unitSphere

spec :: Spec
spec = do
  describe "hit" $ do
    it "hit, all positive" $ do
        let i1 = Intersection 1 (vec 0 0 0)
        let i2 = Intersection 2 (vec 0 0 0)
        let xs = sortIntersections [i1, i2]
        hit xs `shouldBe` Just i1
    it "hit, some negative" $ do
        let i1 = Intersection (-1) (vec 0 0 0)
        let i2 = Intersection 2 (vec 0 0 0)
        let xs = sortIntersections [i1, i2]
        hit xs `shouldBe` Just i2
    it "hit, all negative" $ do
        let i1 = Intersection (-2) (vec 0 0 0)
        let i2 = Intersection (-1) (vec 0 0 0)
        let xs = sortIntersections [i1, i2]
        hit xs `shouldBe` Nothing
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
    it "normal on x-axis" $ do
        let s = unitSphere
        let n = normal' s (pnt 1 0 0)
        n `shouldBe` vec 1 0 0
    it "normal on y-axis" $ do
        let s = unitSphere
        let n = normal' s (pnt 0 1 0)
        n `shouldBe` vec 0 1 0
    it "normal on z-axis" $ do
        let s = unitSphere
        let n = normal' s (pnt 0 0 1)
        n `shouldBe` vec 0 0 1
    it "normal on non-axis" $ do
        let a = sqrt 3 / 3
        let s = unitSphere
        let n = normal' s (pnt a a a)
        n `shouldBe` vec a a a
    it "normal on translated" $ do
        let m = transMat (pnt 0 1 0)
        let s = Sphere 1 m
        let n = normal' s (pnt 0 1.70711 (-0.70711))
        n `shouldBe` vec 0 0.70711 (-0.70711)


