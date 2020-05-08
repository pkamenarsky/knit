{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Eot.Eot (
  HasEotG(..),
  Named(..)
  ) where

import           Data.Proxy
import           Data.Void
import           GHC.Generics
import           GHC.TypeLits

-- * datatype

class HasEotG (a :: * -> *) where
  type EotG a :: *
  toEotG :: a x -> EotG a
  fromEotG :: EotG a -> a x

instance HasConstructorsG f => HasEotG (D1 c f) where
  type EotG (D1 c f) = Constructors f
  toEotG (M1 x) = toEotConstructors x
  fromEotG x = M1 $ fromEotConstructors x

-- * constructors

class HasConstructorsG (a :: * -> *) where
  type Constructors a :: *
  toEotConstructors :: a x -> Constructors a
  fromEotConstructors :: Constructors a -> a x

instance (HasConstructorsG a, HasConstructorsG b, Normalize (Constructors a) (Constructors b)) =>
  HasConstructorsG (a :+: b) where
    type Constructors (a :+: b) = GEither (Constructors a) (Constructors b)
    toEotConstructors = \ case
      L1 a -> gLeft (toEotConstructors a) (Proxy :: Proxy (Constructors b))
      R1 b -> gRight (Proxy :: Proxy (Constructors a)) (toEotConstructors b)
    fromEotConstructors x = case gEither x of
      Left a -> L1 (fromEotConstructors a)
      Right b -> R1 (fromEotConstructors b)

instance HasFieldsG f => HasConstructorsG (C1 c f) where
  type Constructors (C1 c f) = Either (Fields f) Void
  toEotConstructors = Left . toEotFields . unM1
  fromEotConstructors = \ case
    Left fields -> M1 $ fromEotFields fields
    Right void -> absurd void

instance HasConstructorsG V1 where
  type Constructors V1 = Void
  toEotConstructors v1 = seq v1 (error "impossible")
  fromEotConstructors = absurd

-- * GEither

class Normalize a b where
  type GEither a b :: *
  gLeft :: a -> Proxy b -> GEither a b
  gRight :: Proxy a -> b -> GEither a b
  gEither :: GEither a b -> Either a b

instance Normalize b c => Normalize (Either a b) c where
  type GEither (Either a b) c = Either a (GEither b c)
  gLeft (Left a) Proxy = Left a
  gLeft (Right b) Proxy = Right $ gLeft b (Proxy :: Proxy c)
  gRight Proxy c = Right $ gRight (Proxy :: Proxy b) c
  gEither :: Either a (GEither b c) -> Either (Either a b) c
  gEither = \ case
    Left a -> Left (Left a)
    Right g -> case gEither g of
      Left b -> Left (Right b)
      Right c -> Right c

instance Normalize Void b where
  type GEither Void b = b
  gLeft void Proxy = absurd void
  gRight Proxy b = b
  gEither :: b -> Either Void b
  gEither = Right

-- * fields

class HasFieldsG (a :: * -> *) where
  type Fields a :: *
  toEotFields :: a x -> Fields a
  fromEotFields :: Fields a -> a x

instance (HasFieldsG a, HasFieldsG b, Concat (Fields a) (Fields b)) =>
  HasFieldsG (a :*: b) where
    type Fields (a :*: b) = Fields a +++ Fields b
    toEotFields (a :*: b) = toEotFields a +++ toEotFields b
    fromEotFields x = case unConcat x of
      (a, b) -> fromEotFields a :*: fromEotFields b

data Named (a :: Symbol) field = Named field deriving Show
data Unnamed field = Unnamed field deriving Show

instance KnownSymbol name => HasFieldsG (S1 ('MetaSel ('Just name) x y z) (Rec0 f)) where
  type Fields (S1 ('MetaSel ('Just name) x y z) (Rec0 f)) = (Named name f, ())
  toEotFields (M1 (K1 x)) = (Named x, ())
  fromEotFields (Named x, ()) = M1 $ K1 x

instance HasFieldsG (S1 ('MetaSel 'Nothing x y z) (Rec0 f)) where
  type Fields (S1 ('MetaSel 'Nothing x y z) (Rec0 f)) = (Unnamed f, ())
  toEotFields (M1 (K1 x)) = (Unnamed x, ())
  fromEotFields (Unnamed x, ()) = M1 $ K1 x

instance HasFieldsG U1 where
  type Fields U1 = ()
  toEotFields U1 = ()
  fromEotFields () = U1

-- * heterogenous lists

class Concat a b where
  type a +++ b :: *
  (+++) :: a -> b -> (a +++ b)
  unConcat :: (a +++ b) -> (a, b)

instance Concat as bs => Concat (a, as) bs where
  type (a, as) +++ bs = (a, as +++ bs)
  (a, as) +++ bs = (a, as +++ bs)
  unConcat :: (a, as +++ bs) -> ((a, as), bs)
  unConcat (a, rest) = case unConcat rest of
    (as, bs) -> ((a, as), bs)

instance Concat () bs where
  type () +++ bs = bs
  () +++ bs = bs
  unConcat :: bs -> ((), bs)
  unConcat bs = ((), bs)
