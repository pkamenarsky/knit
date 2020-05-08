{-# OPTIONS -Wno-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import GHC.Generics
import Knit

-- Stolen from https://hackage.haskell.org/package/tie-knot-0.2/docs/Data-Knot.html

data Person tables m = Person
  { name  :: Id tables m String
  , loves :: [ForeignId tables m "persons" "name"]
  } deriving (Generic, KnitRecord Model)

deriving instance Show (Person Model Resolved)

data Model m = Model
  { persons :: Table Model m Person
  } deriving (Generic, KnitTables)

deriving instance Show (Model Resolved)

--------------------------------------------------------------------------------

model :: Model Unresolved
model = Model
  [ Person (Id "Alice") [ ForeignId "Bob", ForeignId "cat" ]
  , Person (Id "Bob") [ ForeignId "Alice" ]

  -- You may disagree, but the cat thinks of itself as Person
  , Person (Id "cat") [ ForeignId "cat" ]
  ]

knitModel :: Model Resolved
knitModel = case knit model of
  Right resolved -> resolved
  Left e -> error (show e)

manualModel :: Model Resolved
manualModel = Model
  [ alice
  , bob
  , cat
  ]
  where
    alice = Person "Alice" [ Lazy bob, Lazy cat ]
    bob = Person "Bob" [ Lazy alice ]
    cat = Person "cat" [ Lazy cat ]

--------------------------------------------------------------------------------

model2 :: Model Unresolved
model2 = Model
  [ Person (Remove "Alice") [ ForeignId "Bob", ForeignId "cat" ]
  , Person (Id "Bob") [ ForeignId "Alice" ]

  -- You may disagree, but the cat thinks of itself as Person
  , Person (Id "cat") [ ForeignId "cat" ]
  ]

knitModel2 :: Model Resolved
knitModel2 = case knit model2 of
  Right resolved -> resolved
  Left e -> error (show e)

manualModel2 :: Model Resolved
manualModel2 = Model
  [ cat
  ]
  where
    cat = Person "cat" [ Lazy cat ]

--------------------------------------------------------------------------------

whoLovesX :: Model Resolved -> String -> [String]
whoLovesX m x =
  [ lovingName
  | Person lovingName lovedPersons <- persons m
  , lovedPerson <- lovedPersons
  , name (get lovedPerson) == x
  ]

testModel :: Model Resolved -> Model Resolved -> IO ()
testModel km mm = sequence_
  [ if whoLovesX km x == whoLovesX mm x
      then putStrLn $ "  Testing " <> x <> ": passed"
      else error $ "  Testing " <> x <> ": failed"
  | x <- [ "Bob", "Alice", "cat" ]
  ]

main :: IO ()
main = do
  putStrLn "Testing model..."
  testModel knitModel manualModel

  putStrLn "Testing model2..."
  testModel knitModel2 manualModel2
