{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module TypeSearch.Database.Backend.PostgreSQL
  ( newDbBuilder,
    newDbReader,
    migrate,
  )
where

import Control.Foldl qualified as Foldl
import Data.ByteString qualified as BS
import Data.Int
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Monoid.Do qualified as Monoid
import Data.Text qualified as T
import Data.Vector qualified as V
import Flat
import Hasql.Connection
import Hasql.Decoders qualified as Decoders
import Hasql.DynamicStatements.Snippet qualified as Snippet
import Hasql.DynamicStatements.Statement
import Hasql.Encoders qualified as Encoders
import Hasql.Migration
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Hasql.Transaction.Sessions
import Paths_dependent_type_search
import System.FilePath
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Database.Backend hiding (loadByAnyFeature, resolveNames)
import TypeSearch.Database.Feature
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Migration

migrate :: Connection -> IO ()
migrate conn = void do
  dataDir <- getDataDir
  let migrationDir = dataDir </> "migration"
  migrationCmds <- loadMigrationsFromDirectory migrationDir
  flip run conn $ transactionNoRetry ReadCommitted Write do
    traverse runMigration (MigrationInitialization : migrationCmds)

--------------------------------------------------------------------------------
-- Types

data DbLibraryItemRow = DbLibraryItemRow
  { canonicalName :: T.Text,
    signature :: BS.ByteString,
    body :: Maybe BS.ByteString,
    arity :: Int16,
    arityHasVar :: Bool,
    polymorphic :: T.Text,
    returnTypeHead :: T.Text,
    returnTypeHeadTop :: Maybe T.Text
  }

data DbExportRow = DbExportRow
  { canonicalName :: T.Text,
    exportAsQual :: T.Text,
    exportAsUnqual :: T.Text
  }

--------------------------------------------------------------------------------
-- Encode/decode

encodeQName :: QName -> T.Text
encodeQName (QName m x) = coerce m <> "." <> coerce x

decodeQName :: T.Text -> Either T.Text QName
decodeQName txt = do
  let (mod, name) = first T.init $ T.breakOnEnd "." txt
  when (T.null mod || T.null name) do
    throwError $ "Bad QName: " <> txt
  pure $ QName (coerce mod) (coerce name)

nonNullQNameEnc :: Encoders.NullableOrNot Encoders.Value QName
nonNullQNameEnc = Encoders.nonNullable $ encodeQName >$< Encoders.text

nonNullQNameDec :: Decoders.NullableOrNot Decoders.Value QName
nonNullQNameDec = Decoders.nonNullable $ Decoders.refine decodeQName Decoders.text

encodeTerm :: Term -> BS.ByteString
encodeTerm = flat

decodeTerm :: BS.ByteString -> Either T.Text Term
decodeTerm bin = unflat bin ??% const "Bad flat binary"

nonNullTermDec :: Decoders.NullableOrNot Decoders.Value Term
nonNullTermDec = Decoders.nonNullable $ Decoders.refine decodeTerm Decoders.bytea

encodePolymorphic :: Polymorphic -> T.Text
encodePolymorphic = \case
  Monomorphic -> "Monomorphic"
  Polymorphic -> "Polymorphic"

nonNullPolymorphicEnc :: Encoders.NullableOrNot Encoders.Value Polymorphic
nonNullPolymorphicEnc = Encoders.nonNullable $ encodePolymorphic >$< Encoders.text

encodeReturnTypeHeadTag :: ReturnTypeHead n -> T.Text
encodeReturnTypeHeadTag = \case
  RHTop {} -> "Top"
  RHU -> "U"
  RHVar -> "Var"
  RHSigma -> "Sigma"
  RHProj1 -> "Proj1"
  RHProj2 -> "Proj2"
  RHUnknown -> "Unknown"

encodeReturnTypeHeadTop :: ReturnTypeHead QName -> Maybe T.Text
encodeReturnTypeHeadTop = \case
  RHTop name -> Just $ encodeQName name
  _ -> Nothing

nonNullReturnTypeHeadTagEnc :: Encoders.NullableOrNot Encoders.Value (ReturnTypeHead n)
nonNullReturnTypeHeadTagEnc = Encoders.nonNullable $ encodeReturnTypeHeadTag >$< Encoders.text

--------------------------------------------------------------------------------
-- Build operation

newDbBuilder :: Connection -> DbBuilder IO ()
newDbBuilder conn = Foldl.FoldM step begin done
  where
    begin = orThrow $ flip run conn do
      sql "TRUNCATE library_items, exports RESTART IDENTITY;"

    step () LibraryFragment {..} = orThrow $ flip run conn $ pipeline do
      Pipeline.statement definitions insertManyDefinitions
        *> Pipeline.statement exports insertManyExports

    done () = orThrow $ flip run conn do
      sql
        """
        REFRESH MATERIALIZED VIEW exports_unqual;
        REFRESH MATERIALIZED VIEW exports_qual;
        """

insertManyDefinitions :: Statement [Definition] ()
insertManyDefinitions = lmap encodeDefinitions do
  [resultlessStatement|
    INSERT INTO "library_items"
      ( canonical_name
      , signature
      , body
      , arity
      , arity_has_var
      , polymorphic
      , return_type_head
      , return_type_head_top )
    SELECT * FROM UNNEST
      ( $1 :: text[]
      , $2 :: bytea[]
      , $3 :: bytea?[]
      , $4 :: int2[]
      , $5 :: boolean[]
      , $6 :: text[] :: polymorphic[]
      , $7 :: text[] :: return_type_head[]
      , $8 :: text?[] )
  |]
  where
    encodeDefinitions = unzip8 . V.map (adapt . encodeDefinition) . V.fromList
    adapt DbLibraryItemRow {..} =
      ( canonicalName,
        signature,
        body,
        arity,
        arityHasVar,
        polymorphic,
        returnTypeHead,
        returnTypeHeadTop
      )

insertManyExports :: Statement [Export] ()
insertManyExports = lmap encodeExports do
  [resultlessStatement|
    INSERT INTO "exports"
      ( canonical_name
      , export_as_qual
      , export_as_unqual )
    SELECT * FROM UNNEST
      ( $1 :: text[]
      , $2 :: text[]
      , $3 :: text[] )
  |]
  where
    encodeExports = V.unzip3 . V.map (adapt . encodeExport) . V.fromList
    adapt DbExportRow {..} =
      ( canonicalName,
        exportAsQual,
        exportAsUnqual
      )

encodeDefinition :: Definition -> DbLibraryItemRow
encodeDefinition def = DbLibraryItemRow {..}
  where
    canonicalName = encodeQName def.name
    signature = encodeTerm def.signature
    body = encodeTerm <$> def.body
    arity = fromIntegral def.feature.arity.arity
    arityHasVar = def.feature.arity.hasVar
    polymorphic = encodePolymorphic def.feature.polymorphic
    returnTypeHead = encodeReturnTypeHeadTag def.feature.returnTypeHead
    returnTypeHeadTop = encodeReturnTypeHeadTop def.feature.returnTypeHead

encodeExport :: Export -> DbExportRow
encodeExport export = DbExportRow {..}
  where
    canonicalName = encodeQName export.canonicalName
    exportAsQual = encodeQName export.exportAs
    exportAsUnqual = coerce export.exportAs.name

--------------------------------------------------------------------------------
-- Read operation

newDbReader :: Connection -> DbReader IO
newDbReader conn =
  DbReader
    { resolveNames = resolveNames conn,
      loadByAnyFeature = loadByAnyFeature conn
    }

resolveNames :: (Traversable t) => Connection -> t PQName -> IO (t [Referent])
resolveNames conn names = do
  let names' = toList names
      (qualNames, unqualNames) = partitionEithers $ pqNameToEither <$> names'
  (resolQual, resolUnqual) <- orThrow $ flip run conn $ pipeline do
    liftA2
      (,)
      do Pipeline.statement qualNames loadReferentQual
      do Pipeline.statement unqualNames loadReferentUnqual
  pure $! flip fmapDefault names \case
    Qual m x -> resolQual M.! QName m x
    Unqual x -> resolUnqual M.! x

loadReferentQual :: Statement [QName] (M.Map QName [Referent])
loadReferentQual = lmap encodeQNames $ refineResult decodeReferents do
  [foldStatement|
    SELECT e.export_as_qual :: text, e.canonical_name :: text, i.body :: bytea?
    FROM library_items i
    JOIN exports_qual e
      ON i.canonical_name = e.canonical_name
    WHERE e.export_as_qual = ANY($1 :: text[])
  |]
    groupReferents
  where
    encodeQNames = V.map encodeQName . V.fromList
    decodeReferents =
      traverse (traverse decodeReferent)
        . M.mapKeysMonotonic (fromRight impossible . decodeQName)
    groupReferents =
      lmap (\(exportAs, canonName, body) -> (exportAs, (canonName, body))) do
        Foldl.foldByKeyMap Foldl.list

loadReferentUnqual :: Statement [Name] (M.Map Name [Referent])
loadReferentUnqual = lmap encodeNames $ refineResult decodeReferents do
  [foldStatement|
    SELECT e.export_as_unqual :: text, e.canonical_name :: text, i.body :: bytea?
    FROM library_items i
    JOIN exports_unqual e
      ON i.canonical_name = e.canonical_name
    WHERE e.export_as_unqual = ANY($1 :: text[])
  |]
    groupReferents
  where
    encodeNames = coerce @_ @(V.Vector T.Text) . V.fromList
    decodeReferents = traverse (traverse decodeReferent)
    groupReferents =
      lmap (\(exportAs, canonName, body) -> (coerce exportAs, (canonName, body))) do
        Foldl.foldByKeyMap Foldl.list

decodeReferent :: (T.Text, Maybe BS.ByteString) -> Either T.Text Referent
decodeReferent (canonicalName, body) = do
  canonicalName <- decodeQName canonicalName
  body <- traverse decodeTerm body
  pure $! Referent {..}

loadByAnyFeature :: (Foldable t) => Connection -> t (Compat AllFeature) -> IO [LibraryItem]
loadByAnyFeature conn feats = case NE.nonEmpty (toList feats) of
  Nothing -> pure []
  Just feats -> loadByAnyFeatureNE conn feats

loadByAnyFeatureNE :: Connection -> NE.NonEmpty (Compat AllFeature) -> IO [LibraryItem]
loadByAnyFeatureNE conn compats = orThrow $ flip run conn do
  statement () $ dynamicallyParameterized snippet decoder False
  where
    snippet =
      """
      SELECT
        i.canonical_name,
        i.signature,
        COALESCE (e.reexported_as, '{}') :: text[] AS reexported_as
      FROM library_items i
      LEFT JOIN (
        SELECT
          canonical_name,
          array_agg(DISTINCT export_as_qual)
            FILTER (
              WHERE export_as_qual IS NOT NULL
                AND export_as_qual <> canonical_name
            ) AS reexported_as
        FROM exports_qual
        GROUP BY canonical_name
      ) e
        ON e.canonical_name = i.canonical_name
      WHERE
      """
        <> featuresSnippet compats

    decoder = Decoders.rowList do
      canonicalName <- Decoders.column nonNullQNameDec
      signature <- Decoders.column nonNullTermDec
      reexportedAs <- Decoders.column $ Decoders.nonNullable $ Decoders.listArray nonNullQNameDec
      pure $! LibraryItem {..}

featuresSnippet :: NE.NonEmpty (Compat AllFeature) -> Snippet.Snippet
featuresSnippet = orS . fmap featureSnippet

featureSnippet :: Compat AllFeature -> Snippet.Snippet
featureSnippet AllFeatureCompat {..} =
  andS $
    aritySnippet arity
      NE.:| catMaybes
        [ polymorphicSnippet polymorphic,
          returnTypeHeadSnippet returnTypeHead
        ]

polymorphicSnippet :: Compat Polymorphic -> Maybe Snippet.Snippet
polymorphicSnippet = \case
  AnyPoly -> Nothing
  IsPoly -> Just Monoid.do
    "polymorphic = "
    parS Monoid.do
      Snippet.encoderAndParam nonNullPolymorphicEnc Polymorphic
      " :: polymorphic"

aritySnippet :: Compat Arity -> Snippet.Snippet
aritySnippet = \case
  HasVar -> "arity_has_var"
  HasVarOrGe arity ->
    orS
      [ "arity_has_var",
        "arity >= " <> Snippet.param @Int16 (fromIntegral arity)
      ]

returnTypeHeadSnippet :: Compat (ReturnTypeHead QName) -> Maybe Snippet.Snippet
returnTypeHeadSnippet = \case
  AnyReturnType -> Nothing
  IsVar -> Just isVar
  IsVarOrU -> Just $ isVarOr RHU
  IsVarOrSigma -> Just $ isVarOr RHSigma
  IsVarOrProj1 -> Just $ isVarOr RHProj1
  IsVarOrProj2 -> Just $ isVarOr RHProj2
  IsVarOrTop n -> Just $ orS [isVar, isTop n]
  where
    enc rh = parS Monoid.do
      Snippet.encoderAndParam nonNullReturnTypeHeadTagEnc rh
      " :: return_type_head"

    var = enc RHVar
    isVar = "return_type_head = " <> var
    isVarOr rh = "return_type_head IN " <> listS [var, enc rh]
    isTop n =
      andS
        [ "return_type_head = " <> enc (RHTop ()),
          "return_type_head_top = " <> Snippet.encoderAndParam nonNullQNameEnc n
        ]

parS :: Snippet.Snippet -> Snippet.Snippet
parS s = "(" <> s <> ")"

orS :: NE.NonEmpty Snippet.Snippet -> Snippet.Snippet
orS ss = fmap parS ss `sepBy` " OR "

andS :: NE.NonEmpty Snippet.Snippet -> Snippet.Snippet
andS ss = fmap parS ss `sepBy` " AND "

listS :: NE.NonEmpty Snippet.Snippet -> Snippet.Snippet
listS ss = parS $ ss `sepBy` ", "

--------------------------------------------------------------------------------
-- Utils

pqNameToEither :: PQName -> Either QName Name
pqNameToEither = \case
  Qual m x -> Left (QName m x)
  Unqual x -> Right x

unzip8 ::
  V.Vector (a, b, c, d, e, f, g, h) ->
  (V.Vector a, V.Vector b, V.Vector c, V.Vector d, V.Vector e, V.Vector f, V.Vector g, V.Vector h)
unzip8 xs =
  ( V.map (\(a, _, _, _, _, _, _, _) -> a) xs,
    V.map (\(_, b, _, _, _, _, _, _) -> b) xs,
    V.map (\(_, _, c, _, _, _, _, _) -> c) xs,
    V.map (\(_, _, _, d, _, _, _, _) -> d) xs,
    V.map (\(_, _, _, _, e, _, _, _) -> e) xs,
    V.map (\(_, _, _, _, _, f, _, _) -> f) xs,
    V.map (\(_, _, _, _, _, _, g, _) -> g) xs,
    V.map (\(_, _, _, _, _, _, _, h) -> h) xs
  )
{-# INLINE unzip8 #-}
