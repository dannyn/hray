module SceneSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Math
import           Ray
import           Scene
import           Colour
import           Sphere

import           Linear

main :: IO ()
main = hspec spec

imat = Material (colour 0 0 1) 0 0 0 0 
iray = Ray (pnt 0 0 0) (vec 0 0 1)
inorm = \x -> vec 1 0 0 

imaterial = Material (colour 1 1 1) 0.1 0.9 0.9 200.0
originPnt = pnt 0 0 0

ispheres = map sphere [ Sphere m1 (identity :: M44 Double)
                      , Sphere m2 s]
    where m1 = Material (colour 0.8 1.0 0.6) 0.1 0.7 0.2 200.0
          m2 = Material (colour 1 1 1) 0.1 0.9 0.9 200.0
          s = scaleMat $ pnt 0.5 0.5 0.5

tIntersection t = Intersection t iray n imaterial
    where n = normal' unitSphere

iscene = Scene ispheres l (pnt 0 0 (-5)) 1000 7 10
    where l = Light (pnt (-10) 10 (-10)) (colour 1 1 1)

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
  describe "scene" $ do
    it "two spheres" $ do
        let r = Ray (pnt 0 0 (-5)) (vec 0 0 1)
        let i = map tIntersection [4, 4.5, 5.5, 6]
        intersectShapes ispheres r `shouldBe` i
    it "prepare computations" $ do
        let r = Ray (pnt 0 0 (-5)) (vec 0 0 1)
        let n = normal' unitSphere
        let i = Intersection 4 r n imaterial
        let c = IntComps 4 (pnt 0 0 (-1)) (vec 0 0 (-1)) (vec 0 0 (-1)) False
        (prepareComps i) `shouldBe` c
    it "intersection outside" $ do
        let r = Ray (pnt 0 0 (-5)) (vec 0 0 1)
        let n = normal' unitSphere
        let i = Intersection 4 r n imaterial
        let c = prepareComps i
        let isInside = \(IntComps _ _ _ _ inside) -> inside
        isInside c `shouldBe` False
    it "intersection inside" $ do
        let r = Ray (pnt 0 0 0) (vec 0 0 1)
        let n = normal' unitSphere
        let i = Intersection 1 r n imaterial
        let c = prepareComps i
        let expectedc = IntComps 1 (pnt 0 0 1) (vec 0 0 (-1)) (vec 0 0 (-1)) True
        c `shouldBe` expectedc
    --it "shading intersection" $ do
    --    let r = Ray (pnt 0 0 0) (vec 0 0 1)
    --    let n = normal' unitSphere
    --    let i = Intersection 4 r n imaterial
    --    let c = prepareComps i
