{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Examples.DocsSpec where

import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Test.Hspec

import           Docs
import           Generics.Eot

spec :: Spec
spec = do
  describe "docs" $ do
    it "works" $ do
      docs (Proxy :: Proxy Test) `stringCompare` unindent [i|
        Datatype Test has 3 constructors:

        1. Constructor "A" with the following fields:
          - [Char] (example: "foo")
          - () (example: ())
        2. Constructor "B" with the following fields:
          - a :: Bool (example: True)
          - b :: Either () [Char] (example: Right "foo")
        3. Constructor "C" with no fields
      |]

stringCompare :: String -> String -> IO ()
stringCompare a b
  | prefix <= 3 = a `shouldBe` b
  | otherwise =
      let f s = "..." ++ drop (prefix - 10) s
      in f a `shouldBe` f b
  where
    prefix = prefixH 0 a b
    prefixH n x y = case (x, y) of
      (a : as, b : bs) | a == b ->
        prefixH (succ n) as bs
      _ -> n

data Test
  = A String ()
  | B { a :: Bool, b :: Either () String }
  | C
  deriving (Generic)

instance HasSample Bool where
  sample = True

instance HasSample String where
  sample = "foo"

instance HasSample b => HasSample (Either a b) where
  sample = Right sample

instance HasSample () where
  sample = ()
