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

data Person tables m = Person
  { name :: String
  , friend :: Maybe (ForeignId tables m "persons" "pid")
  , employer :: Maybe (ForeignId tables m "employers" "owner")
  , pid :: Id tables m Word64
  , pid2 :: Id tables m String
  , other :: Employer tables m
  } deriving (G.Generic, KnitRecord CompanyTables)

deriving instance Show (Person CompanyTables 'Unresolved)
deriving instance Show (Person CompanyTables 'Resolved)

data MaybeList a = MaybeList [Maybe a]
  deriving (Functor, Foldable, G.Generic, Show)

data Employer tables m = Employer
  { address :: String
  , employees :: MaybeList (ForeignId tables m "persons" "pid")
  , owner :: Id tables m String
  } deriving (G.Generic, KnitRecord CompanyTables)

deriving instance Show (Employer CompanyTables 'Unresolved)
deriving instance Show (Employer CompanyTables 'Resolved)

data CompanyTables m = CompanyTables
  { persons :: Table CompanyTables m Person
  , employers :: Table CompanyTables m Employer
  } deriving (G.Generic, KnitTables)

deriving instance Show (CompanyTables 'Resolved)
