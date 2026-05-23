{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}

module TypeSearch.Database.Backend.PostgreSQL
  ( newDbBuilder,
    migrate,
  )
where

import Control.Exception
import Data.ByteString qualified as BS
import Data.Int
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
import TypeSearch.Core.Name
import TypeSearch.Database.Backend
import TypeSearch.Database.Feature
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- API

newDbBuilder :: (MonadIO m) => Connection -> DbBuilder m
newDbBuilder conn =
  DbBuilder
    { build = build conn
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

--------------------------------------------------------------------------------
-- Build operation

convertDefinition :: Definition -> DbLibraryItem
convertDefinition Definition {..} =
  DbLibraryItem
    { canonicalName = T.show name,
      signature = flat signature,
      body = flat <$> body,
      arity = fromIntegral feature.arity.arity,
      arityHasVar = feature.arity.hasVar,
      polymorphic = T.show feature.polymorphic,
      returnTypeHead = case feature.returnTypeHead of
        RHU -> "U"
        RHVar -> "Var"
        RHTop {} -> "Top"
        RHSigma -> "Sigma"
        RHProj1 -> "Proj1"
        RHProj2 -> "Proj2"
        RHUnknown -> "Unknown",
      returnTypeHeadTop = case feature.returnTypeHead of
        RHTop n -> Just (T.show n)
        _ -> Nothing
    }

convertExport :: Export -> DbExport
convertExport Export {..} =
  DbExport
    { canonicalName = T.show canonicalName,
      exportAsQual = T.show exportAs,
      exportAsUnqual = T.show exportAs.name
    }

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

insertManyItems :: Statement (V.Vector DbLibraryItem) ()
insertManyItems = lmap (unzip8 . V.map adapt) do
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

insertManyExports :: Statement (V.Vector DbExport) ()
insertManyExports = lmap (V.unzip3 . V.map adapt) do
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
    let items' = V.map convertDefinition $ V.fromList definitions
        exports' = V.map convertExport $ V.fromList exports
    result <- flip run conn $ pipeline do
      Hasql.Pipeline.statement items' insertManyItems
      Hasql.Pipeline.statement exports' insertManyExports
      pure ()
    -- FIXME: better exception handling
    either throwIO pure result
  result <- liftIO $ flip run conn do
    sql
      """
      REFRESH MATERIALIZED VIEW unqual_name_resolution;
      REFRESH MATERIALIZED VIEW qual_name_resolution;
      """
  liftIO $ either throwIO pure result
