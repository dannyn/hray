module SceneSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Math
import           Ray
import           Scene
import           Colour

import           Linear

main :: IO ()
main = hspec spec

s = sphere unitSphere

imat = Material (colour 0 0 1) 0 0 0 0 
iray = Ray (pnt 0 0 0) (vec 0 0 1)
inorm = \x -> vec 1 0 0 

imaterial = Material (colour 1 1 1) 0.1 0.9 0.9 200.0
originPnt = pnt 0 0 0

spec :: Spec
spec = do
  describe "hit" $ do
    it "hit, all positive" $ do
        let i1 = Intersection 1 iray inorm imat
        let i2 = Intersection 2 iray inorm imat
        let xs = sortIntersections [i1, i2]
        hit xs `shouldBe` Just i1
    it "hit, some negative" $ do
        let i1 = Intersection (-1) iray inorm imat
        let i2 = Intersection 2 iray inorm imat
        let xs = sortIntersections [i1, i2]
        hit xs `shouldBe` Just i2
    it "hit, all negative" $ do
        let i1 = Intersection (-2) iray inorm imat
        let i2 = Intersection (-1) iray inorm imat
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
        let s = Sphere imaterial m
        intersects s r `shouldBe` [3, 7]
    it "scaled intersection" $ do
        let r = Ray (pnt 0 0 (-5)) (vec 0 0 1)
        let m = transMat (pnt 5 0 0)
        let s = Sphere imaterial m
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
        let s = Sphere imaterial m
        let n = normal' s (pnt 0 1.70711 (-0.70711))
        vecCmp n (vec 0 0.7071068 (-0.7071068)) `shouldBe` True
  describe "lighting" $ do
    it "between light and surface" $ do
        let eyev = vec 0 0 (-1)
        let normalv = vec 0 0 (-1)
        let light = Light (pnt 0 0 (-10)) (colour 1 1 1)
        let c = lighting imaterial light originPnt eyev normalv 
        let result = colour 1.9 1.9 1.9
        nearZero (c - result) `shouldBe` True
    it "between light and surface, 45 degree offset" $ do
        let r = sqrt 2 / 2
        let eyev = vec 0 r (-r)
        let normalv = vec 0 0 (-1)
        let light = Light (pnt 0 0 (-10)) (colour 1 1 1)
        let c = lighting imaterial light originPnt eyev normalv 
        let result = colour 1.0 1.0 1.0
        nearZero (c - result) `shouldBe` True
    it "opposite surface, 45 degree offset" $ do
        let eyev = vec 0 0 (-1)
        let normalv = vec 0 0 (-1)
        let light = Light (pnt 0 10 (-10)) (colour 1 1 1)
        let c = lighting imaterial light originPnt eyev normalv 
        let r = 0.1 + (0.9 * (sqrt 2/2))
        let result = colour r r r
        nearZero (c - result) `shouldBe` True
    it "in path of reflection vector" $ do
        let r = sqrt 2 / 2
        let eyev = vec 0 (-r) (-r)
        let normalv = vec 0 0 (-1)
        let light = Light (pnt 0 10 (-10)) (colour 1 1 1)
        let c = lighting imaterial light originPnt eyev normalv 
        let rs = 0.1 + (0.9 * (sqrt 2/2)) + 0.9
        let result = colour rs rs rs
        nearZero (c - result) `shouldBe` True
    it "behind surface" $ do
        let eyev = vec 0 0 (-1)
        let normalv = vec 0 0 (-1)
        let light = Light (pnt 0 0 (10)) (colour 1 1 1)
        let c = lighting imaterial light originPnt eyev normalv 
        let result = colour 0.1 0.1 0.1
        nearZero (c - result) `shouldBe` True 
