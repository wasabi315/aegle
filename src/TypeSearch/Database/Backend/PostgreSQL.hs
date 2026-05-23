{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}

module TypeSearch.Database.Backend.PostgreSQL
  ( newDbBuilder,
    newDbReader,
    migrate,
  )
where

import Control.Exception
import Control.Foldl qualified as Foldl
import Control.Lens
import Data.ByteString qualified as BS
import Data.Int
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Flat
import Hasql.Connection
import Hasql.Migration
import Hasql.Pipeline qualified
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import Hasql.Transaction.Sessions
import ListT qualified
import Paths_dependent_type_search
import System.FilePath
import Text.Read
import TypeSearch.Core.Name
import TypeSearch.Database.Backend hiding (loadByAnyFeature, resolveNames)
import TypeSearch.Database.Feature
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- API

newDbBuilder :: (MonadIO m) => Connection -> DbBuilder m
newDbBuilder conn =
  DbBuilder
    { build = build conn
    }

newDbReader :: (MonadIO m) => Connection -> DbReader m
newDbReader conn =
  DbReader
    { resolveNames = resolveNames conn,
      loadByAnyFeature = loadByAnyFeature conn
    }

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
  let (m, x) = first T.init $ T.breakOnEnd "." txt
  if T.null m || T.null x
    then Nothing
    else Just $ QName (coerce m) (coerce x)

flatBytes :: forall a. (Flat a) => Prism' BS.ByteString a
flatBytes = prism' flat \bin -> case unflat @a bin of
  Right t -> Just t
  Left _ -> Nothing

polymorphicDb :: Prism' T.Text Polymorphic
polymorphicDb = prism' T.show (readMaybe . T.unpack)

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

--------------------------------------------------------------------------------
-- Build operation

convertDefinition :: Definition -> DbLibraryItem
convertDefinition def = DbLibraryItem {..}
  where
    canonicalName = qnameText # def.name
    signature = flatBytes # def.signature
    body = review flatBytes <$> def.body
    arity = fromIntegral def.feature.arity.arity
    arityHasVar = def.feature.arity.hasVar
    polymorphic = polymorphicDb # def.feature.polymorphic
    (returnTypeHead, returnTypeHeadTop) =
      returnTypeHeadDb # def.feature.returnTypeHead

convertExport :: Export -> DbExport
convertExport export = DbExport {..}
  where
    canonicalName = qnameText # export.canonicalName
    exportAsQual = qnameText # export.exportAs
    exportAsUnqual = _Unwrapped' # export.exportAs.name

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
      , $6 :: text[] ::polymorphic[]
      , $7 :: text[] ::return_type_head[]
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

build :: (MonadIO m) => Connection -> ListT.ListT m LibraryFragment -> m ()
build conn fragments = do
  result <- liftIO $ flip run conn do
    sql "TRUNCATE library_items, exports RESTART IDENTITY;"
  liftIO $ either throwIO pure result
  flip ListT.traverse_ fragments \LibraryFragment {..} -> liftIO do
    result <- flip run conn $ pipeline do
      Hasql.Pipeline.statement definitions insertManyDefinitions
      Hasql.Pipeline.statement exports insertManyExports
      pure ()
    -- FIXME: better exception handling
    either throwIO pure result
  result <- liftIO $ flip run conn do
    sql
      """
      REFRESH MATERIALIZED VIEW exports_unqual;
      REFRESH MATERIALIZED VIEW exports_qual;
      """
  liftIO $ either throwIO pure result

--------------------------------------------------------------------------------
-- Read operation

pqNameToEither :: PQName -> Either QName Name
pqNameToEither = \case
  Qual m x -> Left (QName m x)
  Unqual x -> Right x

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

resolveNames ::
  (MonadIO m, Traversable t) => Connection -> t PQName -> m (t [Referent])
resolveNames conn names = do
  let names' = toList names
      (qualNames, unqualNames) = partitionEithers $ pqNameToEither <$> names'
  result <- liftIO $ flip run conn $ pipeline do
    resolQual <- Hasql.Pipeline.statement qualNames loadReferentQual
    resolUnqual <- Hasql.Pipeline.statement unqualNames loadReferentUnqual
    pure (resolQual, resolUnqual)
  (resolQual, resolUnqual) <- liftIO $ either throwIO pure result
  pure $! flip fmapDefault names \case
    Qual m x -> resolQual M.! QName m x
    Unqual x -> resolUnqual M.! x

loadByAnyFeature ::
  (MonadIO m, Foldable t) => Connection -> t (Feature QName) -> ListT.ListT m LibraryItem
loadByAnyFeature _conn _feats = mempty
