module Laws (
    semigroupAssocLeft
  , semigroupAssocRight
  , monoidAssocLeft
  , monoidAssocRight
  , monoidLeftIdentity
  , monoidRightIdentity
    ) where

semigroupAssocLeft :: Semigroup m => m -> m -> m -> m
semigroupAssocLeft a b c = ((a <> b) <> c)

semigroupAssocRight :: Semigroup m => m -> m -> m -> m
semigroupAssocRight a b c = (a <> (b <> c))

monoidAssocLeft :: Monoid m => m -> m -> m -> m
monoidAssocLeft a b c = ((a <> b) <> c)

monoidAssocRight :: Monoid m => m -> m -> m -> m
monoidAssocRight a b c = (a <> (b <> c))

monoidLeftIdentity :: Monoid m => m -> m
monoidLeftIdentity a = mempty <> a

monoidRightIdentity :: Monoid m => m -> m
monoidRightIdentity a = a <> mempty
