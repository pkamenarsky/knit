{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ToString (ToString(..)) where

import           Data.List
import           Generics.Eot

class ToString a where
  toString :: a -> String
  default toString :: (HasEot a, ToStringG (Eot a)) => a -> String
  toString = toStringG

instance ToString Bool where
  toString = show

instance ToString Int where
  toString = show

instance ToString () where
  toString = show

toStringG :: forall a . (HasEot a, ToStringG (Eot a)) => a -> String
toStringG a =
  toStringConss
    (constructors (datatype (Proxy :: Proxy a)))
    (toEot a)

class ToStringG a where
  toStringConss :: [Constructor] -> a -> String

instance (ToStringFields a, ToStringG b) => ToStringG (Either a b) where
  toStringConss (Constructor name fieldMeta : _) (Left fields) =
    name ++ format (toStringFields fields)
    where
      format fieldStrings = case fieldMeta of
        Selectors names ->
          " {" ++ intercalate ", "
            (zipWith (\ sel v -> sel ++ " = " ++ v) names fieldStrings) ++
          "}"
        NoSelectors _ -> " " ++ unwords fieldStrings
        NoFields -> ""
  toStringConss (_ : r) (Right next) =
    toStringConss r next
  toStringConss [] _ = error "impossible"

instance ToStringG Void where
  toStringConss _ = absurd

class ToStringFields a where
  toStringFields :: a -> [String]

instance (ToString x, ToStringFields xs) => ToStringFields (x, xs) where
  toStringFields (x, xs) = toString x : toStringFields xs

instance ToStringFields () where
  toStringFields () = []
