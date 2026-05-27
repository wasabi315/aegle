{-# LANGUAGE QuasiQuotes #-}

module TypeSearch.Database.Backend.PostgreSQL
  ( newDbBuilder,
    newDbReader,
    migrate,
  )
where

import Control.Foldl qualified as Foldl
import Control.Lens
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
import Text.Read (readMaybe)
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

data DbLibraryItem = DbLibraryItem
  { canonicalName :: T.Text,
    signature :: BS.ByteString,
    body :: Maybe BS.ByteString,
    arity :: Int16,
    arityHasVar :: Bool,
    polymorphic :: T.Text,
    returnTypeHead :: T.Text,
    returnTypeHeadTop :: Maybe T.Text
  }

data DbExport = DbExport
  { canonicalName :: T.Text,
    exportAsQual :: T.Text,
    exportAsUnqual :: T.Text
  }

data DbReferent = DbReferent
  { canonicalName :: T.Text,
    body :: Maybe BS.ByteString
  }

--------------------------------------------------------------------------------
-- Encode/decode

qnameText :: Prism' T.Text QName
qnameText = prism' T.show \txt -> do
  let (mod, name) = first T.init $ T.breakOnEnd "." txt
  guard $ not (T.null mod) && not (T.null name)
  pure $ QName (coerce mod) (coerce name)

qnameEncoder :: Encoders.Value QName
qnameEncoder = review qnameText >$< Encoders.text

qnameDecoder :: Decoders.Value QName
qnameDecoder =
  flip Decoders.refine Decoders.text \txt ->
    (txt ^? qnameText) ??: ("Ill-formed QName: " <> txt)

flatBytes :: forall a. (Flat a) => Prism' BS.ByteString a
flatBytes = prism' flat $ either (const Nothing) Just . unflat @a

flatBytesDecoder :: forall a. (Flat a) => Decoders.Value a
flatBytesDecoder =
  flip Decoders.refine Decoders.bytea \bin ->
    (bin ^? flatBytes) ??: "Ill-formed flat binary"

polymorphicEnum :: Prism' T.Text Polymorphic
polymorphicEnum = prism' T.show (readMaybe . T.unpack)

returnTypeHeadDb :: Prism' (T.Text, Maybe T.Text) (ReturnTypeHead QName)
returnTypeHeadDb =
  prism'
    ( \case
        RHTop n -> ("Top", Just $ qnameText # n)
        RHU -> ("U", Nothing)
        RHVar -> ("Var", Nothing)
        RHSigma -> ("Sigma", Nothing)
        RHProj1 -> ("Proj1", Nothing)
        RHProj2 -> ("Proj2", Nothing)
        RHUnknown -> ("Unknown", Nothing)
    )
    ( \case
        ("Top", Just n) -> RHTop <$> n ^? qnameText
        ("U", Nothing) -> Just RHU
        ("Var", Nothing) -> Just RHVar
        ("Sigma", Nothing) -> Just RHSigma
        ("Proj1", Nothing) -> Just RHProj1
        ("Proj2", Nothing) -> Just RHProj2
        ("Unknown", Nothing) -> Just RHUnknown
        _ -> Nothing
    )

returnTypeHeadTagEncoder :: Encoders.Value (ReturnTypeHead QName)
returnTypeHeadTagEncoder = view (re returnTypeHeadDb . _1) >$< Encoders.text

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

convertDefinition :: Definition -> DbLibraryItem
convertDefinition def = DbLibraryItem {..}
  where
    canonicalName = qnameText # def.name
    signature = flatBytes # def.signature
    body = review flatBytes <$> def.body
    arity = fromIntegral def.feature.arity.arity
    arityHasVar = def.feature.arity.hasVar
    polymorphic = polymorphicEnum # def.feature.polymorphic
    (returnTypeHead, returnTypeHeadTop) =
      returnTypeHeadDb # def.feature.returnTypeHead

convertExport :: Export -> DbExport
convertExport export = DbExport {..}
  where
    canonicalName = qnameText # export.canonicalName
    exportAsQual = qnameText # export.exportAs
    exportAsUnqual = _Unwrapped' # export.exportAs.name

insertManyDefinitions :: Statement [Definition] ()
insertManyDefinitions =
  lmap (V.map convertDefinition . V.fromList) insertManyDbItems

insertManyDbItems :: Statement (V.Vector DbLibraryItem) ()
insertManyDbItems = lmap (unzip8 . V.map adapt) do
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
    adapt DbLibraryItem {..} =
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
insertManyExports =
  lmap (V.map convertExport . V.fromList) insertManyDbExports

insertManyDbExports :: Statement (V.Vector DbExport) ()
insertManyDbExports = lmap (V.unzip3 . V.map adapt) do
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
    adapt DbExport {..} =
      (canonicalName, exportAsQual, exportAsUnqual)

--------------------------------------------------------------------------------
-- Read operation

newDbReader :: Connection -> DbReader IO
newDbReader conn =
  DbReader
    { resolveNames = resolveNames conn,
      loadByAnyFeature = loadByAnyFeature conn
    }

resolveNames ::
  (Traversable t) => Connection -> t PQName -> IO (t [Referent])
resolveNames conn names = do
  let names' = toList names
      (qualNames, unqualNames) = partitionEithers $ pqNameToEither <$> names'
  (resolQual, resolUnqual) <- orThrow $ flip run conn $ pipeline do
    (,)
      <$> Pipeline.statement qualNames loadReferentQual
      <*> Pipeline.statement unqualNames loadReferentUnqual
  pure $! flip fmapDefault names \case
    Qual m x -> resolQual M.! QName m x
    Unqual x -> resolUnqual M.! x

loadByAnyFeature ::
  (Foldable t) => Connection -> t (Feature QName) -> IO [LibraryItem]
loadByAnyFeature conn feats = case NE.nonEmpty (toList feats) of
  Nothing -> pure []
  Just feats -> do
    let snippet = Monoid.do
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
          featuresSnippet feats
        decoder = Decoders.rowList do
          canonicalName <- Decoders.column $ Decoders.nonNullable qnameDecoder
          signature :: Term <- Decoders.column $ Decoders.nonNullable flatBytesDecoder
          reexportedAs <- Decoders.column $ Decoders.nonNullable $ Decoders.listArray $ Decoders.nonNullable qnameDecoder
          pure $! LibraryItem {..}

    orThrow $ flip run conn do
      statement () $ dynamicallyParameterized snippet decoder False

convertDbReferent :: DbReferent -> Either T.Text Referent
convertDbReferent DbReferent {..} = do
  canonicalName <-
    (canonicalName ^? qnameText) ??: ("Ill-formed QName: " <> canonicalName)
  body <- for body \body ->
    (body ^? flatBytes) ??: "Ill-formed Term"
  pure $! Referent {..}

loadReferentQual :: Statement [QName] (M.Map QName [Referent])
loadReferentQual =
  lmap (V.map (review qnameText) . V.fromList) $
    refineResult
      (traverse (traverse convertDbReferent) . M.mapKeysMonotonic (^?! qnameText))
      loadReferentQual'

loadReferentUnqual :: Statement [Name] (M.Map Name [Referent])
loadReferentUnqual =
  lmap (V.map (view _Wrapped') . V.fromList) $
    refineResult
      (traverse (traverse convertDbReferent) . M.mapKeysMonotonic (view _Unwrapped'))
      loadReferentUnqual'

referentAggregation :: Foldl.Fold (T.Text, T.Text, Maybe BS.ByteString) (M.Map T.Text [DbReferent])
referentAggregation =
  lmap (\(exportAs, canonName, body) -> (exportAs, (canonName, body))) do
    Foldl.foldByKeyMap (lmap (uncurry DbReferent) Foldl.list)

loadReferentQual' :: Statement (V.Vector T.Text) (M.Map T.Text [DbReferent])
loadReferentQual' =
  [foldStatement|
    SELECT e.export_as_qual :: text, e.canonical_name :: text, i.body :: bytea?
    FROM library_items i
    JOIN exports_qual e
      ON i.canonical_name = e.canonical_name
    WHERE e.export_as_qual = ANY($1 :: text[])
  |]
    referentAggregation

loadReferentUnqual' :: Statement (V.Vector T.Text) (M.Map T.Text [DbReferent])
loadReferentUnqual' =
  [foldStatement|
    SELECT e.export_as_unqual :: text, e.canonical_name :: text, i.body :: bytea?
    FROM library_items i
    JOIN exports_unqual e
      ON i.canonical_name = e.canonical_name
    WHERE e.export_as_unqual = ANY($1 :: text[])
  |]
    referentAggregation

featuresSnippet :: NE.NonEmpty (Feature QName) -> Snippet.Snippet
featuresSnippet feats =
  fmap featureSnippet feats `sepBy` " OR "

featureSnippet :: Feature QName -> Snippet.Snippet
featureSnippet Feature {..} =
  "(" <> (snippets `sepBy` " AND ") <> ")"
  where
    snippets =
      aritySnippet arity
        NE.:| catMaybes
          [ polymorphicSnippet polymorphic,
            returnTypeHeadSnippet returnTypeHead
          ]

polymorphicSnippet :: Polymorphic -> Maybe Snippet.Snippet
polymorphicSnippet = \case
  Monomorphic -> Nothing
  Polymorphic -> Just "polymorphic = 'Polymorphic'"

aritySnippet :: Arity -> Snippet.Snippet
aritySnippet arity | arity.hasVar = "arity_has_var"
aritySnippet arity = Monoid.do
  "(arity_has_var OR arity >= "
  Snippet.param @Int16 (fromIntegral arity.arity)
  ")"

returnTypeHeadSnippet :: ReturnTypeHead QName -> Maybe Snippet.Snippet
returnTypeHeadSnippet = \case
  RHUnknown -> Nothing
  RHTop n -> Just Monoid.do
    """
    (return_type_head = 'Var' OR
      (return_type_head = 'Top' AND return_type_head_top =
    """
    Snippet.encoderAndParam (Encoders.nonNullable qnameEncoder) n
    "))"
  r -> Just Monoid.do
    "return_type_head IN ('Var', ("
    Snippet.encoderAndParam (Encoders.nonNullable returnTypeHeadTagEncoder) r
    " :: return_type_head))"

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
