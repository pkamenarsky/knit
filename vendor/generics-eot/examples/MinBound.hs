{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module MinBound (minBoundG) where

import           Generics.Eot

minBoundG :: (HasEot a, GMinBound (Eot a)) => a
minBoundG = fromEot gMinBound

class GMinBound a where
  gMinBound :: a

instance GMinBound a => GMinBound (Either a x) where
  gMinBound = Left gMinBound

instance (Bounded f, GMinBound fs) =>
  GMinBound (f, fs) where
    gMinBound = (minBound, gMinBound)

instance GMinBound () where
  gMinBound = ()
