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
