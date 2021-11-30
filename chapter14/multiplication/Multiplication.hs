-- Multiplication.hs
module Multiplication where

import Test.Hspec

multipleBy :: (Eq a, Num a) => a -> a -> a
multipleBy x y = go x y
    where go a b
            | b == 0 = 0
            | otherwise = a + go a (b - 1)


main :: IO ()
main = hspec $ do
    describe "Multiplication" $ do
        it "4 multipled by 10 is 40" $ do
            multipleBy 4 10 `shouldBe` (40 :: Integer)
        it "10 multipled by 0 is 0" $ do
            multipleBy 10 0 `shouldBe` (0 :: Integer)
        it "0 multipled by 10 is 0" $ do
            multipleBy 0 10 `shouldBe` (0 :: Integer)