module ExampleSpec where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "foo" $ do
        let
            f = id
        it "id" $ property $
            \x -> f x `shouldBe` (x :: Int)
    describe "bar" $ do
        it "id" $ property $
            \x -> x `shouldBe` (x :: Int)