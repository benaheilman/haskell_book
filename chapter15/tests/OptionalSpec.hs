module OptionalSpec where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

import Laws
import Optional

spec :: Spec
spec = do
    it "Nada is Nada" $ do
        Nada `shouldBe` (Nada :: Optional Int)
    it "associative" $ property $
        \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: Optional (Sum Int))
    it "left identity" $ property $
        \x -> monoidLeftIdentity x `shouldBe` (x :: Optional (Sum Int))
    it "right identity" $ property $
        \x -> monoidRightIdentity x `shouldBe` (x :: Optional (Sum Int))