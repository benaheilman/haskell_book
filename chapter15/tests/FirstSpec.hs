module FirstSpec where

import Test.Hspec
import Test.QuickCheck

import Laws
import First

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

spec :: Spec
spec = do
    it "associative" $ property $
        \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: First' String)
    it "left identity" $ property $
        \x -> monoidLeftIdentity x `shouldBe` (x :: First' String)
    it "right identity" $ property $
        \x -> monoidRightIdentity x `shouldBe` (x :: First' String)