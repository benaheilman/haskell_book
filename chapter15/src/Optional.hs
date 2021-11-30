module Optional (
    Optional (Nada, Only)
    ) where

import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Semigroup (Optional a) where
    (<>) (Only x) (Only y) = Only (x <> y)
    (<>) Nada (Only y) = Only y
    (<>) (Only x) Nada = Only x
    (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = do
    a <- arbitrary
    oneof [return $ (Only a), return $ Nada]
    
instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = optionalGen