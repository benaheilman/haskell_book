module LawsSpec where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

import Laws

spec :: Spec
spec = do
    describe "()" $ do
        it "associative" $ property $
            \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: ())
        it "left identity" $ property $
            \x -> monoidLeftIdentity x `shouldBe` (x :: ())
        it "right identity" $ property $
            \x -> monoidRightIdentity x `shouldBe` (x :: ())
    describe "Sum Int" $ do
        it "associative" $ property $
            \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: Sum Int)
        it "left identity" $ property $
            \x -> monoidLeftIdentity x `shouldBe` (x :: Sum Int)
        it "right identity" $ property $
            \x -> monoidRightIdentity x `shouldBe` (x :: Sum Int)