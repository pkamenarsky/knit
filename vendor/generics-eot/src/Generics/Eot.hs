{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | @generics-eot@ tries to be a library for datatype generic programming
-- that is easy to understand. "eot" stands for "eithers of tuples".
--
-- A tutorial on how to use @generics-eot@ can be found here:
-- https://generics-eot.readthedocs.io/.
module Generics.Eot (
  HasEot(..),
  Named(..),

  -- * Meta Information
  Datatype(..),
  Constructor(..),
  Fields(..),

  -- * Useful Re-exports
  Generic,
  Proxy(..),
  Void,
  absurd,
  ) where

import           Data.Proxy
import           Data.Void
import           GHC.Exts (Constraint)
import           GHC.Generics hiding (Datatype, Constructor)

import           Generics.Eot.Datatype
import           Generics.Eot.Eot

-- | An instance (@'HasEot' a@) allows us to
--
-- - convert values of an arbitrary algebraic datatype @a@ to and from a generic
--   representation (@'Eot' a@) (see 'toEot' and 'fromEot').
-- - extract meta information about the type @a@ (see 'datatype').
--
-- Once an algebraic datatype has an instance for 'GHC.Generics.Generic' it
-- automatically gets one for 'HasEot'.
class HasEot a where
  -- | 'Eot' is a type level function that maps arbitrary ADTs to isomorphic
  -- generic representations. Here's an example:
  --
  -- > data Foo = A Int Bool | B String
  --
  -- would be mapped to:
  --
  -- > Either (Int, (Bool, ())) (Either (String, ()) Void)
  --
  -- These representations follow these rules:
  --
  -- - The choice between constructors is mapped to right-nested 'Either's.
  -- - There's always a so-called end-marker 'Void'. It's an invalid choice (and
  --   'Void' is uninhabited to make sure you don't accidentally create such a value).
  --   So e.g. @data Foo = A@ would be mapped to @Either () Void@, and a type
  --   with no constructors is mapped to @Void@.
  -- - The fields of one constructor are mapped to right-nested tuples.
  -- - Again there's always an end-marker, this time of type @()@.
  --   A constructor with three fields @a@, @b@, @c@ is mapped to
  --   @(a, (b, (c, ())))@, one field @a@ is mapped to @(a, ())@, and no
  --   fields are mapped to @()@ (just the end-marker).
  --
  -- These rules (and the end-markers) are necessary to make sure generic
  -- functions know exactly which parts of the generic representation are field
  -- types and which parts belong to the generic skeleton.
  type Eot a :: *

  -- | Convert a value of type @a@ to its generic representation.
  toEot :: a -> Eot a

  -- | Convert a value in a generic representation to @a@ (inverse of 'toEot').
  fromEot :: Eot a -> a

  -- | Extract meta information about the ADT.
  datatype :: Proxy a -> Datatype

instance (Generic a, ImpliedByGeneric a c f) => HasEot a where
  type Eot a = EotG (Rep a)
  toEot = toEotG . from
  fromEot = to . fromEotG
  datatype Proxy = datatypeC (Proxy :: Proxy (Rep a))

type family ImpliedByGeneric a c f :: Constraint where
  ImpliedByGeneric a c f =
    (GenericDatatype (Rep a),
     Rep a ~ D1 c f,
     GenericConstructors f,
     HasEotG (Rep a))
