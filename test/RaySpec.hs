module RaySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Math
import Ray

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "canvas" $ do
    it "position" $ do
        let r = Ray (pnt 2 3 4) (vec 1 0 0)
        (pos r (-1)) `shouldBe` (pnt 1 3 4)
  describe "sphere" $ do
    it "intersect at tangent" $ do
        let r = Ray (pnt 0 1 (-5)) (vec 0 0 1)
        (intersects' (Sphere 1) r) `shouldBe` [5.0, 5.0]
    it "misses" $ do
        let r = Ray (pnt 0 2 (-5)) (vec 0 0 1)
        (intersects' (Sphere 1) r) `shouldBe` []
    it "starts inside" $ do
        let r = Ray (pnt 0 0 0) (vec 0 0 1)
        (intersects' (Sphere 1) r) `shouldBe` [(-1.0), 1.0]
    it "behind" $ do
        let r = Ray (pnt 0 0 5) (vec 0 0 1)
        (intersects' (Sphere 1) r) `shouldBe` [(-6.0), (-4.0)]
