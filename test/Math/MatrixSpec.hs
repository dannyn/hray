module Math.MatrixSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector as V

import Math
import Math.Vector as Vec
import Math.Matrix as Mat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "math" $ do
    describe "matrix" $ do
        it "equality" $ do
            let m1 = Mat.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]
            let m2 = Mat.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]
            m1 `shouldBe` m2 
        it "inequality" $ do
            let m1 = Mat.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]
            let m2 = Mat.fromList [2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1]
            m1 `shouldNotBe` m2 
        it "get entry" $ do
            let m = Mat.fromList [1, 2, 3, 4, 5, 6, 7, 8, 8, 9, 7, 6, 5, 4, 3, 2]
            (Mat.getEntry m 1 2) `shouldBe` 9.0
        it "dont get entry" $ do
            let m = Mat.identity
            (Mat.getEntry m 1 1) `shouldNotBe` (Mat.getEntry m 3 2)
        it "matrix multiplication" $ do
            let m1 = Mat.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]
            let m2 = Mat.fromList [(-2), 1, 2, 3, 3, 2, 1, (-1), 4, 3, 6, 5, 1, 2, 7, 8]
            let m3 = Mat.fromList [20, 22, 50, 48, 44, 54, 114, 108, 40, 58, 110, 102, 16, 26, 46, 42]
            (m1 * m2) `shouldBe` m3


