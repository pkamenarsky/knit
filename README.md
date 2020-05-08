# knit

[![CircleCI](https://circleci.com/gh/pkamenarsky/knit.svg?style=svg)](https://circleci.com/gh/pkamenarsky/knit)

`knit` ties the knot on data structures that reference each other by unique keys. Above all it aims to be easy to use - boilerplate is kept to a minimum and its API is as simple as it gets.

## Example

```haskell
data Person model m = Person
  { name        :: Id model m String
  , loves       :: ForeignId model m "persons" "name" --
  , isPresident :: Bool                               --
  } deriving (Generic, KnitRecord Model)              --
                                                      -- 
data Model m = Model                                  --
  { persons :: Table Model m Person -- <----------------
  } deriving (Generic, KnitTables)
```

Let's break that down: when defining a domain type, like `Person`, we'll need two additional type parameters that will determine the final shape of that type: the `model` `Person` belongs to (it may belong to multiple models), and its "mode" (`m`) - whether it's *resolved* or *unresolved*. Additionally, we need to derive `KnitRecord` for every domain type, supplying it with a concrete `model` type.

`Id model m t` will define a key this type is referenced by (multiple keys are possible).

`ForeignId` is where the magic happens - in addition to the two generic parameters from above it takes a "table" name (which is just a field in the `model`) and a field name in the referenced domain type; the final type of the `ForeignId` field (both resolved and unresolved) can then be inferred from this information alone!

To define a `model`, wrap each domain type with a `Table` and autoderive the `KnitTables` typeclass.

Let's take a look:

```haskell
alice :: Person Model 'Unresolved
alice = Person
  { name        = Id "Alice"
  , loves       = ForeignId "Bob"  -- this must be a String, since Model.persons.name is a String!
  , isPresident = False
  }

bob :: Person Model 'Unresolved
bob = Person
  { name        = Id "Bob"
  , loves       = ForeignId "Alice"
  , isPresident = False
  }

model :: Model 'Unresolved
model = Model
  { persons = [alice, bob]  -- `Table` is just a regular list
  }
```

So far so good. Resolving an unresolved model is just a matter of calling `knit`:

```haskell
knitModel :: Model Resolved
knitModel = case knit model of
  Right resolved -> resolved
  Left e -> error (show e)
```

(`knit` may fail due to invalid or duplicate keys). If all goes well, we'll get the following *resolved* model, if we were to do it by hand:

```haskell
manualAlice :: Person Model 'Resolved
manualAlice = Person
  { name        = "Alice"
  , loves       = Lazy manualBob
  , isPresident = False
  }

manualBob :: Person Model 'Resolved
manualBob = Person
  { name        = "Bob"
  , loves       = Lazy manualAlice
  , isPresident = False
  }

manualModel :: Model 'Resolved
manualModel = Model
  { persons = [manualAlice, manualBob]
  }
```

`Lazy` is just a simple wrapper with a `get` field:

```haskell
data Lazy a = { get :: a }
```

And here it is, a nicely knit model:

```haskell
name $ get $ loves (persons knitModel !! 0) -- "Bob"
```

The `test` directory contains more examples, with multiple domain types.

## Cascading deletes

By supplying a `Remove` key instead the regular `Id` a record is marked for deletion:

```haskell
alice :: Person Model 'Unresolved
alice = Person
  { name        = Remove "Alice"  -- mark the record for deletion
  , loves       = ForeignId "Bob"
  , isPresident = False
  }
```

This will remove the record from the resolved result, as well as *all other records that depend transitively on it*. Invalid keys (i.e. `ForeignId`s that reference non-existent `Id`s) will still throw an error when `knit`-ting a model.
