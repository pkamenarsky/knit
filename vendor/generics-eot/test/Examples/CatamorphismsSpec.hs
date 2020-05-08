{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.CatamorphismsSpec where

import           Data.Proxy
import           Generics.Eot
import           Test.Hspec

import           Catamorphisms

spec :: Spec
spec = do
  describe "catamorphism" $ do
    it "works for one constructor, no fields" $ do
      catamorphism A (Proxy :: Proxy String) "foo"
        `shouldBe` "foo"

    it "works for one constructor, one field" $ do
      catamorphism (B 42) (Proxy :: Proxy String)
        show `shouldBe` "42"

    it "works for two constructors, no fields" $ do
      catamorphism C2 (Proxy :: Proxy String)
        "1" "2" `shouldBe` "2"

    it "works for more complex types" $ do
      let cata :: forall a . D -> (Int -> a) -> a -> (String -> Bool -> a) -> a
          cata d = catamorphism d (Proxy :: Proxy a)
          f d = cata d show "unit" (\ a b -> show (a, b))
      f (D1 42) `shouldBe` "42"
      f D2 `shouldBe` "unit"
      f (D3 "foo" True) `shouldBe` "(\"foo\",True)"

data A
  = A
  deriving (Generic)

data B
  = B Int
  deriving (Generic)

data C
  = C1 | C2
  deriving (Generic)

data D
  = D1 Int
  | D2
  | D3 String Bool
  deriving (Generic)
