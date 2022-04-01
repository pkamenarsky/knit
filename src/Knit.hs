{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Knit
  ( KnitRecord
  , KnitTables
  , Mode (..)

  , Lazy (..)

  , Id
  , ForeignId

  , RecordId (..)
  , ForeignRecordId (..)

  , ResolveError (..)

  , knit

  , ResolveErrorR (..)
  , Resolver (..)
  , ResolveRecord (..)
  , RecordTable (..)
  )where

import           Control.DeepSeq (NFData)
import qualified Control.Monad.ST as ST

import           Data.Bifunctor (first)
import           Data.Foldable (Foldable, toList)
import qualified Data.HashTable.Class as HC
import qualified Data.HashTable.ST.Basic as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))

import           GHC.Generics (Generic)
import           GHC.TypeLits (KnownSymbol, Symbol, TypeError, ErrorMessage(..), symbolVal)
import qualified Generics.Eot as Eot
import           Generics.Eot (Eot, HasEot, Named (Named), Void, Proxy (..), fromEot, toEot)

import           Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

type family Fst a where
  Fst '(a, b) = a

type family Snd a where
  Snd '(a, b) = b

--------------------------------------------------------------------------------

type TableName = String
type FieldName = String
type FieldValue = String

data Mode = Resolved | Unresolved | Done

data RecordId t = Id t | Remove t
  deriving (Show, Eq, Ord, Generic)

instance NFData t => NFData (RecordId t)

type family Id (tables :: Mode -> *) (recordMode :: Mode) t where
  Id tables 'Done t = RecordId t
  Id tables 'Resolved t = t
  Id tables 'Unresolved t = RecordId t

data Lazy tables a = Lazy
  { get :: a tables 'Resolved
  }

instance Show (Lazy tables a) where
  show _ = "Lazy"

newtype ForeignRecordId (table :: Symbol) (field :: Symbol) t = ForeignId t
  deriving (Show, Eq, Ord, Num, Generic, NFData)

type family ForeignId (tables :: Mode -> *) (recordMode :: Mode) (table :: Symbol) (field :: Symbol) where
  ForeignId tables 'Done table field = ()
  ForeignId tables 'Unresolved table field = ForeignRecordId
    table
    field
    (LookupFieldType field (Snd (LookupTableType table (Eot (tables 'Unresolved)))))
  ForeignId tables 'Resolved table field = Lazy
    tables
    (Fst (LookupTableType table (Eot (tables 'Resolved))))

-- GatherIds -------------------------------------------------------------------

data EId
  = forall t. Show t => EId TableName FieldName t Dynamic
  | forall t. Show t => ERemove TableName FieldName t Dynamic
  | forall t. Show t => EForeignId TableName FieldName t

deriving instance Show EId

newtype Dynamic = Dynamic ()

instance Show Dynamic where
  show _ = "Dynamic"

toDynamic :: a -> Dynamic
toDynamic = Dynamic . unsafeCoerce

fromDynamic :: Dynamic -> a
fromDynamic (Dynamic v) = unsafeCoerce v

--------------------------------------------------------------------------------

class GGatherIds u where
  gGatherIds :: TableName -> Dynamic -> u -> [EId]

instance GGatherIds () where
  gGatherIds _ _ () = []

instance GGatherIds Void where
  gGatherIds _ _ _ = undefined

instance (GGatherIds u, GGatherIds v) => GGatherIds (Either u v) where
  gGatherIds table record (Left u) = gGatherIds table record u
  gGatherIds table record (Right v) = gGatherIds table record v

instance GGatherIds us => GGatherIds (Named field u, us) where
  gGatherIds table record (_, us) = gGatherIds table record us

instance {-# OVERLAPPING #-}
  ( Show t
  , GGatherIds us
  , KnownSymbol field
  ) =>
  GGatherIds (Named field (RecordId t), us) where
    gGatherIds table record (Named (Id k), us)
      = EId table (symbolVal (Proxy :: Proxy field)) k record:gGatherIds table record us
    gGatherIds table record (Named (Remove k), us)
      = ERemove table (symbolVal (Proxy :: Proxy field)) k record:gGatherIds table record us

instance {-# OVERLAPPING #-}
  ( Show t

  , GGatherIds us

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (ForeignRecordId table field t), us) where
    gGatherIds table record (Named (ForeignId k), us)
      = EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) k:gGatherIds table record us

instance {-# OVERLAPPING #-}
  ( Show t

  , GGatherIds us

  , Foldable f

  , KnownSymbol field
  ) =>
  GGatherIds (Named field (f (RecordId t)), us) where
    gGatherIds table record (Named f, us) = eids <> gGatherIds table record us
      where
        eids =
          [ EId table (symbolVal (Proxy :: Proxy field)) k record
          | Id k <- toList f
          ]

instance {-# OVERLAPPING #-}
  ( Show t

  , GGatherIds us

  , Foldable f

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (f (ForeignRecordId table field t)), us) where
    gGatherIds table record (Named f, us) = eids <> gGatherIds table record us
      where
        eids =
          [ EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) k
          | ForeignId k <- toList f
          ]

instance {-# OVERLAPPING #-}
  ( GGatherIds us
  , KnitRecord tables r
  ) =>
  GGatherIds (Named field (r tables 'Unresolved), us) where
    gGatherIds table record (Named r, us)
      = fids <> gGatherIds table record us
      where
        -- only gather foreign ids here, since nested records can't be referenced anyway
        fids =
          [ fid
          | fid@(EForeignId _ _ _) <- gatherIds table (toDynamic r) r
          ]

instance {-# OVERLAPPING #-}
  ( GGatherIds us
  , Foldable f
  , KnitRecord tables r
  ) =>
  GGatherIds (Named field (f (r tables 'Unresolved)), us) where
    gGatherIds table record (Named f, us) = fids <> gGatherIds table record us
      where
        -- only gather foreign ids here, since nested records can't be referenced anyway
        fids = mconcat
          [ [ fid
            | fid@(EForeignId _ _ _) <- gatherIds table (toDynamic r) r
            ]
          | r <- toList f
          ]

-- GatherTableIds --------------------------------------------------------------

class GGatherTableIds t where
  gGatherTableIds :: t -> [(TableName, [[EId]])]

instance GGatherTableIds () where
  gGatherTableIds () = []

instance GGatherTableIds Void where
  gGatherTableIds _ = undefined

instance (GGatherTableIds t, GGatherTableIds u) => GGatherTableIds (Either t u) where
  gGatherTableIds (Left t) = gGatherTableIds t
  gGatherTableIds (Right u) = gGatherTableIds u

instance ( GGatherTableIds ts
         , KnownSymbol table
         ) => GGatherTableIds (Named table a, ts) where
  gGatherTableIds (_, ts) = gGatherTableIds ts

instance {-# OVERLAPPING #-}
  ( GGatherTableIds ts
  , KnitRecord tables r
  , KnownSymbol table
  ) => GGatherTableIds (Named table [r tables 'Unresolved], ts) where
  gGatherTableIds (Named records, ts) = (table, eids):gGatherTableIds ts
    where
      table = symbolVal (Proxy :: Proxy table)
      eids =
        -- TODO: remove table here
        [ gatherIds table (toDynamic record) record
        | record <- records
        ]

-- Resolve ---------------------------------------------------------------------

data ResolveError
  = MissingIds [(TableName, FieldName, FieldValue)]
  | RepeatingIds [(TableName, FieldName, FieldValue)]
  deriving (Eq, Ord, Generic, Show)

instance NFData ResolveError

class GResolve u r where
  gResolve
    :: (TableName -> FieldName -> FieldValue -> Dynamic)
    -> u
    -> r

instance GResolve () () where
  gResolve _ () = ()

instance GResolve Void Void where
  gResolve _ _ = undefined

instance (GResolve u r, GResolve t s) => GResolve (Either u t) (Either r s) where
  gResolve rsvMap (Left u) = Left $ gResolve rsvMap u 
  gResolve rsvMap (Right u) = Right $ gResolve rsvMap u 

instance (GResolve us rs) => GResolve (Named x u, us) (Named x u, rs) where
  gResolve rsvMap (u, us) = (u, gResolve rsvMap us)

instance (GResolve us rs) => GResolve (Named x (RecordId u), us) (Named x u, rs) where
  gResolve rsvMap (Named (Id u), us) = (Named u, gResolve rsvMap us)
  gResolve rsvMap (Named (Remove _), us) = (Named (error "gResolve: Remove: this is a bug"), gResolve rsvMap us)

instance (GResolve us rs, Functor f) => GResolve (Named x (f (RecordId u)), us) (Named x (f u), rs) where
  gResolve rsvMap (Named u', us) = (Named $ fmap (\(Id u) -> u) u', gResolve rsvMap us)

instance
  ( Show u

  , KnitRecord tables r
  , GResolve us rs

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (ForeignRecordId table field u), us) (Named x (Lazy tables r), rs) where
    gResolve rsvMap (Named (ForeignId k), us)
      = ( Named $ Lazy $ resolve rsvMap (fromDynamic $ rsvMap table field (show k))
        , gResolve rsvMap us
        )
      where
        table = symbolVal (Proxy :: Proxy table)
        field = symbolVal (Proxy :: Proxy field)

instance
  ( Show u

  , KnitRecord tables r
  , GResolve us rs

  , Functor f

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (f (ForeignRecordId table field u)), us) (Named x (f (Lazy tables r)), rs) where
    gResolve rsvMap (Named f, us)
      = ( Named $ flip fmap f $ \(ForeignId k) -> Lazy $ resolve rsvMap (fromDynamic $ rsvMap table field (show k))
        , gResolve rsvMap us
        )
      where
        table = symbolVal (Proxy :: Proxy table)
        field = symbolVal (Proxy :: Proxy field)

instance
  ( KnitRecord tables r
  , GResolve us rs
  ) =>
  GResolve (Named x (r tables 'Unresolved), us) (Named x (r tables 'Resolved), rs) where
    gResolve rsvMap (Named u, us) = (Named $ resolve rsvMap u, gResolve rsvMap us)

instance
  ( KnitRecord tables r
  , GResolve us rs
  , Functor f
  ) =>
  GResolve (Named x (f (r tables 'Unresolved)), us) (Named x (f (r tables 'Resolved)), rs) where
    gResolve rsvMap (Named u, us) = (Named $ fmap (resolve rsvMap) u, gResolve rsvMap us)

-- ResolveTables ---------------------------------------------------------------

class GResolveTables u t where
  gResolveTables :: [[Bool]] -> (TableName -> FieldName -> FieldValue -> Dynamic) -> u -> t

instance GResolveTables () () where
  gResolveTables _ _ () = ()

instance GResolveTables u t => GResolveTables (Either u Void) (Either t Void) where
  gResolveTables notRemoved rsvMap (Left u) = Left $ gResolveTables notRemoved rsvMap u
  gResolveTables _ _ _ = undefined

instance GResolveTables us ts => GResolveTables (Named table a, us) (Named table a, ts) where
  gResolveTables nr rsvMap (a, us) = (a, gResolveTables nr rsvMap us)

instance
  ( GResolveTables us ts
  , KnitRecord tables t 
  ) => GResolveTables (Named table [t tables 'Unresolved], us) (Named table [t tables 'Resolved], ts) where
    gResolveTables (notRemoved:notRemoved') rsvMap (Named ts, us)
      = (Named resolved, gResolveTables notRemoved' rsvMap us)
      where
        resolved =
          [ resolve rsvMap t
          | (nr, t) <- zip notRemoved ts
          , nr
          ]
    gResolveTables [] _ _ = error "gResolveTables: [] (this is a bug)"

-- KnitRecord ------------------------------------------------------------------

class KnitRecord (tables :: Mode -> *) u where
  resolve
    :: (TableName -> FieldName -> FieldValue -> Dynamic)
    -> u tables 'Unresolved
    -> u tables 'Resolved
  default resolve
    :: HasEot (u tables 'Unresolved)
    => HasEot (u tables 'Resolved)
    => GResolve (Eot (u tables 'Unresolved)) (Eot (u tables 'Resolved))

    => (TableName -> FieldName -> FieldValue -> Dynamic)
    -> u tables 'Unresolved
    -> u tables 'Resolved
  resolve rsvMap = fromEot . gResolve rsvMap . toEot

  gatherIds :: TableName -> Dynamic -> u tables 'Unresolved -> [EId]
  default gatherIds
    :: HasEot (u tables 'Unresolved)
    => GGatherIds (Eot (u tables 'Unresolved))

    => TableName
    -> Dynamic
    -> u tables 'Unresolved
    -> [EId]
  gatherIds table record = gGatherIds table record . toEot

-- KnitTables ------------------------------------------------------------------

class KnitTables t where
  resolveTables
    :: (TableName -> FieldName -> FieldValue -> Dynamic)
    -> t 'Unresolved
    -> Either ResolveError (t 'Resolved)
  default resolveTables
    :: HasEot (t 'Unresolved)
    => HasEot (t 'Resolved)
    => GResolveTables (Eot (t 'Unresolved)) (Eot (t 'Resolved))
    => KnitTables t

    => (TableName -> FieldName -> FieldValue -> Dynamic)
    -> t 'Unresolved
    -> Either ResolveError (t 'Resolved)
  resolveTables extRsvMap u
    -- | trace dbgInfo False = undefined
    | not (null repeatingIds) = Left $ RepeatingIds repeatingIds
    | [] <- missingIds = Right $ fromEot $ gResolveTables notRemovedIds rsv (toEot u)
    | otherwise = Left $ MissingIds missingIds
    where
      -- dbgInfo = mconcat
      --   [ "Eids: ", show eids, "\n"
      --   , "Not removed ids: ", show notRemovedIds, "\n"
      --   , "Record map: ", show recordMap, "\n"
      --   , "Reverse map: ", show reverseMap, "\n"
      --   , "Removed records: ", show removedRecords, "\n"
      --   , "Repeating ids: ", show repeatingIds, "\n"
      --   , "Missing ids: ", show missingIds, "\n"
      --   ]

      eids = gatherTableIds u

      notRemovedIds =
        [ [ and
              [ case eid of
                  EId table field k _ -> not ((table, field, show k) `S.member` removedRecords)
                  ERemove _ _ _ _ -> False
                  _ -> True
              | eid <- record
              ]
          | record <- records
          ]
        | (_, records) <- eids
        ]

      recordMap = M.fromListWith (<>) $ mconcat
        [ mconcat
            [ case eid of
                EId table field k r -> [((table, field, show k), [(r, True, fids)])]
                ERemove table field k r -> [((table, field, show k), [(r, False, fids)])]
                _ -> []
            | eid <- record
            ]
        | (_, records) <- eids
        , record <- records
        , let fids =
                [ fid
                | fid@(EForeignId _ _ _) <- record
                ]
        ]

      reverseMap = M.fromListWith (<>) $ mconcat
        [ [ ((ftable, ffield, show fk), S.singleton (table, field, k))
          | EForeignId ftable ffield fk <- fids
          ]
        | ((table, field, k), [(_, _, fids)]) <- M.toList recordMap
        ]

      removedRecords = ST.runST $ do
        m <- H.new

        let markRemoved (table, field, k) = do
              v <- H.lookup m (table, field, k)

              case v of
                Nothing -> do
                  H.insert m (table, field, k) True

                  sequence_
                    [ markRemoved (ftable, ffield, fk)
                    | Just fids <- [ M.lookup (table, field, k) reverseMap ]
                    , (ftable, ffield, fk) <- S.toList fids
                    ]
                Just _ -> pure ()

        sequence_
          [ markRemoved k
          | (k, [(_, False, _)]) <- M.toList recordMap
          ]

        S.fromList . fmap fst <$> HC.toList m

      repeatingIds = mconcat
        [ if length records > 1
            then [(table, field, k)]
            else []
        | ((table, field, k), records) <- M.toList recordMap
        ]

      missingIds = catMaybes
        [ case M.lookup (table, field, show k) recordMap of
            Nothing -> Just (table, field, show k)
            Just _ -> Nothing
        | (_, [(_, _, fids)]) <- M.toList recordMap
        , EForeignId table field k <- fids
        ]

      rsvRecord table field value = M.lookup (table, field, value) recordMap

      rsv table field value = case rsvRecord table field value of
        Nothing -> extRsvMap table field value
        Just [(record, _, _)] -> record
        _ -> error "resolveTables: repeating ids (this is a bug, the consistency check should have caught this)"

  gatherTableIds :: t 'Unresolved -> [(TableName, [[EId]])]
  default gatherTableIds
    :: HasEot (t 'Unresolved)
    => GGatherTableIds (Eot (t 'Unresolved))
    => t 'Unresolved
    -> [(TableName, [[EId]])]
  gatherTableIds = gGatherTableIds . toEot

-- Expand ----------------------------------------------------------------------

type family ExpandRecord (parent :: Symbol) (record :: *) where
  ExpandRecord parent () = ()
  ExpandRecord parent (Either fields Eot.Void) = ExpandRecord parent fields
  ExpandRecord parent (Eot.Named name (RecordId a), fields) = (Eot.Named name a, ExpandRecord parent fields)
  ExpandRecord parent (Eot.Named name (f (RecordId a)), fields) = (Eot.Named name (f a), ExpandRecord parent fields)
  ExpandRecord parent (a, fields) = ExpandRecord parent fields

type family LookupTableType (table :: Symbol) (eot :: *) :: (((Mode -> *) -> Mode -> *), *) where
  LookupTableType name (Either records Eot.Void) = LookupTableType name records
  LookupTableType name (Eot.Named name [record tables recordMode], records)
    = '(record, ExpandRecord name (Eot (record tables 'Done)))
  LookupTableType name (Eot.Named otherName a, records)
    = LookupTableType name records

  LookupTableType name eot = TypeError ('Text "Can't lookup table type")

type family LookupFieldType (field :: Symbol) (eot :: *) :: * where
  LookupFieldType name (Either records Eot.Void) = LookupFieldType name records
  LookupFieldType name (Eot.Named name (Maybe field), fields) = field
  LookupFieldType name (Eot.Named name field, fields) = field
  LookupFieldType name (Eot.Named otherName field, fields) = LookupFieldType name fields
  LookupFieldType name eot = TypeError ('Text "Can't lookup field type")

--------------------------------------------------------------------------------

knit :: KnitTables t => t 'Unresolved -> Either ResolveError (t 'Resolved)
knit = resolveTables
  (\tbl k v -> error $ "knit: inconsistent record (this is a bug, the consistency check should have caught this: " <> show tbl <> ", " <> show k <> ", " <> show v)

-- Custom resolvers ------------------------------------------------------------

zipEither :: Semigroup e => (a -> b -> c) -> Either e a -> Either e b -> Either e c
zipEither f (Left e) (Left g) = Left (e <> g)
zipEither f (Left e) (Right _) = Left e
zipEither f (Right _) (Left e) = Left e
zipEither f (Right a) (Right b) = Right (f a b)

sequenceEither :: Semigroup e => [Either e a] -> Either e [a]
sequenceEither [] = Right []
sequenceEither (e:es) = zipEither (:) e (sequenceEither es)

data ResolveErrorR = MissingId T.Text
  deriving (Eq, Ord, Show)

data LazyR (t :: Mode -> *) = LazyR { getR :: t 'Resolved }

instance Show (LazyR a) where
  show _ = "Lazy"

type family Resolver cache (recordMode :: Mode) (t :: Mode -> *) where
  Resolver cache 'Unresolved t = cache -> Either ResolveErrorR (t 'Resolved)
  Resolver tables 'Resolved t = LazyR t

class GResolveRecord cache u t where
  gResolveR :: cache -> u -> Either [ResolveErrorR] t

instance GResolveRecord cache () () where
  gResolveR _ _ = Right ()

instance GResolveRecord cache Void Void where
  gResolveR _ _ = undefined

instance (GResolveRecord cache u r, GResolveRecord cache t s) => GResolveRecord cache (Either u t) (Either r s) where
  gResolveR cache (Left a) = Left <$> gResolveR cache a
  gResolveR cache (Right a) = Right <$> gResolveR cache a

instance (GResolveRecord cache us rs) => GResolveRecord cache (Named x u, us) (Named x u, rs) where
  gResolveR cache (u, us) = (u,) <$> gResolveR cache us

instance (GResolveRecord cache us rs) => GResolveRecord cache (Named x (cache -> Either ResolveErrorR (u 'Resolved)), us) (Named x (LazyR u), rs) where
  gResolveR cache (Named rsv, us) = zipEither (\u us -> (Named (LazyR u), us)) (first (:[]) $ rsv cache) (gResolveR cache us)

instance (ResolveRecord cache r, GResolveRecord cache us rs) => GResolveRecord cache (Named x (r 'Unresolved), us) (Named x (r 'Resolved), rs) where
  gResolveR cache (Named u, us) = zipEither (\u us -> (Named u, us)) (resolveR cache u) (gResolveR cache us)

class ResolveRecord cache t where
  resolveR :: cache -> t 'Unresolved -> Either [ResolveErrorR] (t 'Resolved)
  default resolveR
    :: HasEot (t 'Unresolved)
    => HasEot (t 'Resolved)
    => GResolveRecord cache (Eot (t 'Unresolved)) (Eot (t 'Resolved))

    => cache
    -> t 'Unresolved
    -> Either [ResolveErrorR] (t 'Resolved)
  resolveR cache t = fmap fromEot (gResolveR cache $ toEot t)

--------------------------------------------------------------------------------

class GRecordTable cache u t where
  gResolveRecordTable :: cache -> u -> Either [ResolveErrorR] t

instance GRecordTable cache () () where
  gResolveRecordTable _ _ = Right ()

instance GRecordTable cache Void Void where
  gResolveRecordTable _ _ = undefined

instance (GRecordTable cache u r, GRecordTable cache t s) => GRecordTable cache (Either u t) (Either r s) where
  gResolveRecordTable cache (Left a) = Left <$> gResolveRecordTable cache a
  gResolveRecordTable cache (Right a) = Right <$> gResolveRecordTable cache a

instance (ResolveRecord cache u, GRecordTable cache us rs) => GRecordTable cache (Named x [u 'Unresolved], us) (Named x [u 'Resolved], rs) where
  gResolveRecordTable cache (Named u, us) = zipEither
    (\u us -> (Named u, us))
    (sequenceEither $ map (resolveR cache) u)
    (gResolveRecordTable cache us)

class RecordTable cache t where
  resolveRecordTable :: (t 'Resolved -> cache) -> t 'Unresolved -> Either [ResolveErrorR] (t 'Resolved)
  default resolveRecordTable
    :: HasEot (t 'Unresolved)
    => HasEot (t 'Resolved)
    => GRecordTable cache (Eot (t 'Unresolved)) (Eot (t 'Resolved))

    => (t 'Resolved -> cache)
    -> t 'Unresolved
    -> Either [ResolveErrorR] (t 'Resolved)
  resolveRecordTable buildCache t = rsv
    where
      rsv = fmap fromEot (gResolveRecordTable cache $ toEot t)
      cache = case rsv of
        Left e -> error (show e)
        Right rsv' -> buildCache rsv'
