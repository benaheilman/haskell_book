module Exercises where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend a b = a <> b

instance Arbitrary Trivial where
    arbitrary = return Trivial

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two x y) (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    (<>) (BoolConj a) (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj True

instance Arbitrary BoolConj where
    arbitrary = do
        oneof [return $ BoolConj True, return $ BoolConj False]

newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
    (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

instance Monoid BoolDisj where
    mempty = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = do
        oneof [return $ BoolDisj True, return $ BoolDisj False]

data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
    (<>) (Snd a) _ = Snd a
    (<>) _ (Snd b) = Snd b
    (<>) _ (Fst b) = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a, return $ Snd b]

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show b => Show (Combine a b) where
    show a = "Combine something"

instance Semigroup b => Semigroup (Combine a b) where
    (<>) x y = Combine (\a -> (unCombine x) a <> (unCombine y) a)

-- instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--    arbitrary = do
--        f <- arbitrary
--        return $ Combine { unCombine = f }

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
    (<>) f g = Comp ((unComp f) . (unComp g))

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (<>) (Success' a) _ = Success' a
    (<>) (Failure' _) (Success' b) = Success' b
    (<>) (Failure' a) (Failure' b) = Failure' (a <> b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Failure' a, return $ Success' b]

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
    (<>) f f' = Mem $ g
        where
            g s =
                let
                    (a', s') = (runMem f) s
                    (a'', s'') = (runMem f') s'
                in (a' <> a'', s'')

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \x -> (mempty, x)