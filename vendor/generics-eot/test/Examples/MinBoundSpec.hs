{-# LANGUAGE DeriveGeneric #-}

module Examples.MinBoundSpec where

import           Data.Word
import           Test.Hspec

import           Generics.Eot
import           MinBound

spec :: Spec
spec = do
  describe "minBoundG" $ do
    it "works" $ do
      minBoundG `shouldBe` Foo minBound 0 False

data Foo
  = Foo Int Word8 Bool
  deriving (Eq, Show, Generic)
