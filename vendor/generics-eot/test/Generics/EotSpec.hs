{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generics.EotSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Generics.Eot

spec :: Spec
spec = do
  describe "toEot" $ do
    it "works" $ do
      toEot (A 42 "foo" True ()) `shouldBe`
        Left (42, ("foo", (True, ((), ()))))
      toEot (B True) `shouldBe` Right (Left (True, ()))
      toEot Foo `shouldBe` Left ()

    it "produces the right types" $ do
      let _foo :: Either
            (Int, (String, (Bool, ((), ()))))
            (Either (Bool, ())
             (Either ()
              (Either () Void)))
          _foo = toEot C
      True

  describe "fromEot" $ do
    it "is the inverse of toEot" $ do
      property $ \ eot -> not (isVoid eot) ==> do
        let a :: Test
            a = fromEot eot
        toEot a `shouldBe` eot

    it "the other way around" $ do
      property $ \ (a :: Test) -> do
        let eot :: Eot Test
            eot = toEot a
        fromEot eot `shouldBe` a

instance Arbitrary Void where
  arbitrary = error "please, don't do this!"

class IsVoid a where
  isVoid :: a -> Bool

instance IsVoid (Either b c) => IsVoid (Either a (Either b c)) where
  isVoid = \ case
    Left _ -> False
    Right bc -> isVoid bc

instance IsVoid (Either a Void) where
  isVoid = either (const False) (const True)

data Test
  = A Int String Bool ()
  | B Bool
  | C
  | D
  deriving (Generic, Show, Eq)

instance Arbitrary Test where
  arbitrary = oneof $
    (A <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary) :
    (B <$> arbitrary) :
    (pure C) :
    (pure D) :
    []

data Foo = Foo
  deriving (Generic)
