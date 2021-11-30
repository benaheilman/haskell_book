module First where

import Test.QuickCheck

import Optional

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) (First' (Only x)) _ = First' (Only x)
    (<>) _ (First' (Only y)) = First' (Only y)
    (<>) _ _ = First' Nada

instance Monoid (First' a) where
    mempty = First' Nada

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
    a <- arbitrary
    oneof [return $ (First' (Only a)), return $ (First' Nada)]
    
instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = firstGen