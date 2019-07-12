module ColourSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Linear
import Colour

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Colour" $ do
    it "hadamard product" $ do 
        let c1 = colour 1.0 0.2 0.4
        let c2 = colour 0.9 1.0 0.1
        (nearZero $ (hadamard c1 c2) - (colour 0.9 0.2 0.04)) `shouldBe` True
    it "colour to string" $ do
        let c1 = colour 0.1 0.2 0.3
        colourToRGB c1 `shouldBe` ["25", "51", "76"]