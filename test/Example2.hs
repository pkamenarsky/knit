{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example2 where

import           Data.Word (Word64)

import qualified GHC.Generics as G

import           Knit

data Person m = Person
  { name :: String
  , friend :: Maybe (ForeignId CompanyTables m "persons" "pid")
  , employer :: Maybe (ForeignId CompanyTables m "employers" "owner")
  , pid :: Id m Word64
  , pid2 :: Id m String
  , other :: Employer m
  } deriving (G.Generic, KnitRecord)

deriving instance Show (Person 'Unresolved)
deriving instance Show (Person 'Resolved)

data MaybeList a = MaybeList [Maybe a]
  deriving (Functor, Foldable, G.Generic, Show)

data Employer m = Employer
  { address :: String
  , employees :: MaybeList (ForeignId CompanyTables m "persons" "pid")
  , owner :: Id m String
  } deriving (G.Generic, KnitRecord)

deriving instance Show (Employer 'Unresolved)
deriving instance Show (Employer 'Resolved)

data CompanyTables m = CompanyTables
  { persons :: [Person m]
  , employers :: [Employer m]
  } deriving (G.Generic, KnitTables)

deriving instance Show (CompanyTables 'Resolved)
