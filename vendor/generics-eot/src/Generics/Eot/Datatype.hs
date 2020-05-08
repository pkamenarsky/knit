{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Eot.Datatype where

import           Data.Maybe
import           Data.Proxy
import qualified GHC.Generics as GHC
import           GHC.Generics hiding (Datatype(..), Constructor(..))

-- | Type for meta information about ADTs.
data Datatype
  = Datatype {
    datatypeName :: String, -- ^ unqualified name of the type
    constructors :: [Constructor]
  }
  deriving (Show, Eq)

data Constructor
  = Constructor {
    constructorName :: String,
    fields :: Fields
  }
  deriving (Show, Eq)

-- | Type that represents meta information about fields of one
-- constructor.
data Fields
  = Selectors [String]
    -- ^ Record constructor, containing the list of the selector names.
  | NoSelectors Int
    -- ^ Constructor with fields, but without selector names.
    -- The argument gives the number of fields.
  | NoFields
    -- ^ Constructor without fields.
  deriving (Show, Eq)

-- * datatype

class GenericDatatype (a :: * -> *) where
  datatypeC :: Proxy a -> Datatype

instance (GHC.Datatype c, GenericConstructors f) =>
  GenericDatatype (D1 c f) where
    datatypeC Proxy = Datatype n constructors
      where
        n = GHC.datatypeName (undefined :: D1 c f x)
        constructors = getConstructors (Proxy :: Proxy f)

-- * constructors

class GenericConstructors (a :: * -> *) where
  getConstructors :: Proxy a -> [Constructor]

instance (GenericConstructors a, GenericConstructors b) =>
  GenericConstructors (a :+: b) where
    getConstructors Proxy = getConstructors a ++ getConstructors b
      where
        a :: Proxy a = Proxy
        b :: Proxy b = Proxy

instance (GHC.Constructor c, GenericFields f) =>
  GenericConstructors (C1 c f) where
    getConstructors Proxy = [Constructor n (getFields f)]
      where
        n = GHC.conName (undefined :: (C1 c f x))
        f :: Proxy f = Proxy

instance GenericConstructors V1 where
  getConstructors Proxy = []

-- * fields

getFields :: GenericFields a => Proxy a -> Fields
getFields proxy = case getFieldsC proxy of
  [] -> NoFields
  l@(Nothing : _) -> NoSelectors (length l)
  l@(Just _ : _) -> Selectors (catMaybes l)

class GenericFields (a :: * -> *) where
  getFieldsC :: Proxy a -> [Maybe String]

instance (GenericFields a, GenericFields b) =>
  GenericFields (a :*: b) where
    getFieldsC Proxy = getFieldsC a ++ getFieldsC b
      where
        a :: Proxy a = Proxy
        b :: Proxy b = Proxy

instance Selector c => GenericFields (S1 c (Rec0 f)) where
  getFieldsC proxy = [getField proxy]

getField :: forall c f . Selector c =>
  Proxy (S1 c (Rec0 f)) -> Maybe String
getField Proxy = case selName (undefined :: S1 c (Rec0 f) x) of
  "" -> Nothing
  s -> Just s

instance GenericFields U1 where
  getFieldsC Proxy = []
