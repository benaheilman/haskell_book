module ExercisesSpec where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

import Exercises
import Laws

spec :: Spec
spec = do
    describe "Semigroup" $ do
        context "Trivial" $ do
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: Trivial)

        context "Identity" $ do
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: Identity (Sum Int))

        context "Two" $ do
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: Two (Sum Int) (Sum Int))

        context "Three" $ do
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: Three (Sum Int) (Sum Int) (Sum Int))

        context "Four" $ do
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: Four (Sum Int) (Sum Int) (Sum Int) (Sum Int))

        context "BoolConj" $ do
            it "BoolConj True <> BoolConj True == BoolConj True)" $ do
                BoolConj True <> BoolConj True `shouldBe` BoolConj True
            it "BoolConj True <> BoolConj False == BoolConj False)" $ do
                BoolConj True <> BoolConj False `shouldBe` BoolConj False
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: BoolConj)

        context "BoolDisj" $ do
            it "BoolDisj True <> BoolDisj True == BoolDisj True)" $ do
                BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
            it "BoolDisj True <> BoolDisj False == BoolDisj True)" $ do
                BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: BoolDisj)

        context "Or" $ do
            it "Fst x <> Snd y == Snd y" $ do
                ((Fst 1 <> Snd 2) :: Or Int Int) `shouldBe` Snd (2 :: Int)
            it "Fst x <> Fst y == Fst y" $ do
                ((Fst 1 <> Fst 2) :: Or Int Int) `shouldBe` Fst (2 :: Int)
            it "Snd x <> Fst y == Snd x" $ do
                ((Snd 1 <> Fst 2) :: Or Int Int) `shouldBe` Snd (1 :: Int)
            it "Snd x <> Snd y == Snd x" $ do
                ((Snd 1 <> Snd 2) :: Or Int Int) `shouldBe` Snd (1 :: Int)
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: Or Int Int)

        let
            f :: Combine Integer (Sum Integer)
            f = Combine $ \n -> Sum (n + 1)
            g :: Combine Integer (Sum Integer)
            g = Combine $ \n -> Sum (n - 1)
            in
            context "Combine" $ do
                it "unCombine (f <> g) $ 0 == Sum 0" $ do
                    (unCombine (f <> g) $ 0) `shouldBe` Sum (0 :: Integer)
                it "unCombine (f <> g) $ 1 == Sum 2" $ do
                    (unCombine (f <> g) $ 1) `shouldBe` Sum (2 :: Integer)
                it "unCombine (f <> f) $ 1 == Sum 4" $ do
                    (unCombine (f <> f) $ 1) `shouldBe` Sum (4 :: Integer)
                it "unCombine (g <> f) $ 1 == Sum 2" $ do
                    (unCombine (g <> f) $ 1) `shouldBe` Sum (2 :: Integer)

        let
            f :: Comp (Sum Integer)
            f = Comp $ \n -> n + 1
            g :: Comp (Sum Integer)
            g = Comp $ \n -> n - 1
            in
            context "Comp" $ do
                it "unComp (f <> g) $ 0 == Sum 0" $ do
                    (unComp (f <> g) $ 0) `shouldBe` Sum (0 :: Integer)
                it "unComp (f <> g) $ 1 == Sum 1" $ do
                    (unComp (f <> g) $ 1) `shouldBe` Sum (1 :: Integer)
                it "unComp (f <> f) $ 1 == Sum 3" $ do
                    (unComp (f <> f) $ 1) `shouldBe` Sum (3 :: Integer)
                it "unComp (g <> f) $ 1 == Sum 1" $ do
                    (unComp (g <> f) $ 1) `shouldBe` Sum (1 :: Integer)

        context "Validation" $ do
            it "Success' 1 <> Failure' \"blah\" == Success' 1" $ do
                ((Success' 1 <> Failure' "blah") :: Validation String Int) `shouldBe` Success' 1
            it "Failure' \"woot\" <> Failure' \"blah\" == Failure' \"wootblah\"" $ do
                ((Failure' "woot" <> Failure' "blah") :: Validation String Int) `shouldBe` Failure' "wootblah"
            it "Success' 1 <> Success' 2 == Success' 1" $ do
                ((Success' 1 <> Success' 2) :: Validation String Int) `shouldBe` Success' 1
            it "Failure' \"woot\" <> Success' 2 == Success' 2" $ do
                ((Failure' "woot" <> Success' 2) :: Validation String Int) `shouldBe` Success' 2
            it "associative" $ property $
                \x y z -> semigroupAssocLeft x y z `shouldBe` (semigroupAssocRight x y z :: Validation String (Sum Int))
                

    describe "Monoid" $ do
        context "Trivial" $ do
            it "associative" $ property $
                \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: Trivial)
            it "left identity" $ property $
                \x -> monoidLeftIdentity x `shouldBe` (x :: Trivial)
            it "right identity" $ property $
                \x -> monoidRightIdentity x `shouldBe` (x :: Trivial)

        context "Identity" $ do
            it "associative" $ property $
                \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: Identity (Sum Int))
            it "left Identity" $ property $
                \x -> monoidLeftIdentity x `shouldBe` (x :: Identity (Sum Int))
            it "right identity" $ property $
                \x -> monoidRightIdentity x `shouldBe` (x :: Identity (Sum Int))

        context "Two" $ do
            it "associative" $ property $
                \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: Two (Sum Int) (Sum Int))
            it "left Identity" $ property $
                \x -> monoidLeftIdentity x `shouldBe` (x :: Two (Sum Int) (Sum Int))
            it "right identity" $ property $
                \x -> monoidRightIdentity x `shouldBe` (x :: Two (Sum Int) (Sum Int))

        context "BoolConj" $ do
            it "associative" $ property $
                \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: BoolConj)
            it "left Identity" $ property $
                \x -> monoidLeftIdentity x `shouldBe` (x :: BoolConj)
            it "right identity" $ property $
                \x -> monoidRightIdentity x `shouldBe` (x :: BoolConj)

        context "BoolDisj" $ do
            it "associative" $ property $
                \x y z -> monoidAssocLeft x y z `shouldBe` (monoidAssocRight x y z :: BoolDisj)
            it "left Identity" $ property $
                \x -> monoidLeftIdentity x `shouldBe` (x :: BoolDisj)
            it "right identity" $ property $
                \x -> monoidRightIdentity x `shouldBe` (x :: BoolDisj)

        let
            f' = Mem $ \s -> ("hi", s + 1)
            rmzero = (runMem mempty 0 :: (String, Int))
            rmleft = runMem (f' <> mempty) 0
            rmright = runMem (mempty <> f') 0
            in
            context "Mem" $ do
                it "rmleft == (\"hi\", 1)" $ do
                    rmleft `shouldBe` (("hi", 1) :: (String, Int))
                it "rmright == (\"hi\", 1)" $ do
                    rmright `shouldBe` (("hi", 1) :: (String, Int))
                it "rmzero == (\"\", 0)" $ do
                    rmzero `shouldBe` (("", 0) :: (String, Int))
                it "rmleft == runMem f' 0" $ do
                    rmleft `shouldBe` runMem f' 0
                it "rmright == runMem f' 0" $ do
                    rmright `shouldBe` runMem f' 0
