{-# LANGUAGE DeriveGeneric #-}

module Generics.DatatypeSpec where

import qualified GHC.Generics as GHC
import           Test.Hspec hiding (Selector)

import           Generics.Eot

data TestType
  = A
  | B {
    a :: String
  }
  | C {
    a :: String,
    b :: Int,
    c :: Bool,
    d :: ()
  }
  | D String
  deriving (GHC.Generic, Show)

spec :: Spec
spec = do
  describe "datatype" $ do
    it "returns meta data about record types" $ do
      let expected = Datatype "TestType" $
            Constructor "A" NoFields :
            Constructor "B" (Selectors ["a"]) :
            Constructor "C" (Selectors ["a", "b", "c", "d"]) :
            Constructor "D" (NoSelectors 1) :
            []
      datatype (Proxy :: Proxy TestType) `shouldBe` expected
