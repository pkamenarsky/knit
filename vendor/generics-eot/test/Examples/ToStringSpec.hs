{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.ToStringSpec where

import           Test.Hspec

import           Generics.Eot
import           ToString

data Test
  = A {
    a :: Int,
    b :: (),
    c :: Bool
  }
  | B
  deriving (Generic, ToString)

spec :: Spec
spec = do
  describe "toString" $ do
    it "works" $ do
      toString (A 42 () False) `shouldBe` "A {a = 42, b = (), c = False}"
