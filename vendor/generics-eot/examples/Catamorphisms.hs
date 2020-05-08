{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Catamorphisms where

import           Generics.Eot

catamorphism :: (HasEot a, Cata (Eot a) dest) =>
  a -> Proxy dest -> Typ (Eot a) dest
catamorphism a proxy = cata (toEot a) proxy

class Cata eot dest where
  type Typ eot dest :: *
  cata :: eot -> Proxy dest -> Typ eot dest
  cataConst :: Proxy eot -> dest -> Typ eot dest

instance (Cata rest dest, Cons fields dest) =>
  Cata (Either fields rest) dest where

  type Typ (Either fields rest) dest =
    ConsFunc fields dest -> Typ rest dest
  cata (Left fields) Proxy consFunc =
    cataConst (Proxy :: Proxy rest) $
    (eotConsFunc consFunc fields :: dest)
  cata (Right rest) proxy _ =
    cata rest proxy
  cataConst :: Proxy (Either fields rest) -> dest -> Typ (Either fields rest) dest
  cataConst Proxy dest = const $ cataConst (Proxy :: Proxy rest) dest

instance Cata Void dest where
  type Typ Void dest = dest
  cata :: Void -> Proxy dest -> dest
  cata = absurd
  cataConst Proxy = id

class Cons fields dest where
  type ConsFunc fields dest :: *
  eotConsFunc :: ConsFunc fields dest -> fields -> dest

instance Cons () dest where
  type ConsFunc () dest = dest
  eotConsFunc :: dest -> () -> dest
  eotConsFunc dest () = dest

instance Cons r dest => Cons (a, r) dest where
  type ConsFunc (a, r) dest = a -> ConsFunc r dest
  eotConsFunc :: (a -> ConsFunc r dest) -> (a, r) -> dest
  eotConsFunc consFunc (a, r) =
    eotConsFunc (consFunc a) r
