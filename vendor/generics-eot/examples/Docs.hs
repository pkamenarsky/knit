{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Docs (docs, HasSample(..)) where

import           Data.String.Interpolate
import           Data.Typeable
import           Generics.Eot

docs :: forall a . (HasEot a, GDocs Datatype (Eot a)) => Proxy a -> String
docs Proxy = unlines $ gDocs
  (datatype (Proxy :: Proxy a))
  (Proxy :: Proxy (Eot a))

class GDocs meta eot where
  gDocs :: meta -> Proxy eot -> [String]

-- * datatype

instance GDocs (Int, [Constructor]) x => GDocs Datatype x where
  gDocs (Datatype name constructors) proxy =
    [i|Datatype #{name} has #{length constructors} constructors:|] :
    "" :
    gDocs (1 :: Int, constructors) proxy

-- * constructors

instance (GDocs (Int, Constructor) c, GDocs (Int, [Constructor]) cs) =>
  GDocs (Int, [Constructor]) (Either c cs) where
    gDocs (i, (c : cs)) Proxy =
      gDocs (i, c) (Proxy :: Proxy c) ++
      gDocs (succ i, cs) (Proxy :: Proxy cs)
    gDocs (_, []) Proxy = error "impossible"

instance GDocs (Int, [Constructor]) Void where
  gDocs _ Proxy = []

instance GDocs Fields fields => GDocs (Int, Constructor) fields where
  gDocs (n, Constructor name fields) Proxy = case fields of
    NoFields ->
      [[i|#{n}. Constructor #{show name} with no fields|]]
    _ ->
      [i|#{n}. Constructor #{show name} with the following fields:|] :
      gDocs fields (Proxy :: Proxy fields)

-- * fields

instance GDocs Fields () where
  gDocs _ Proxy = []

instance (Typeable f, HasSample f, Show f, GDocs Fields fs) =>
  GDocs Fields (f, fs) where
    gDocs fields Proxy = case fields of
      Selectors (name : names) ->
        [i|  - #{name} :: #{typeRep f} (example: #{show (sample :: f)})|] :
        gDocs (Selectors names) fs
      Selectors [] -> error "impossible"
      NoSelectors n ->
        [i|  - #{typeRep f} (example: #{show (sample :: f)})|] :
        gDocs (NoSelectors (pred n)) fs
      NoFields -> []
      where
        f = Proxy :: Proxy f
        fs = Proxy :: Proxy fs

-- * HasSample class

class HasSample a where
  sample :: a
