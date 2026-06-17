{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aegle.Index
  ( index,
    Config (..),
    LibraryConfig (..),
    TransparentDefName (..),
  )
where

import Aegle.Database.Backend qualified as TS
import Aegle.Index.Translate (runTransl)
import Aegle.Index.Translate qualified as Transl
import Aegle.Index.Translate.Scope
import Aegle.Index.Utils
import Aegle.Prelude
import Agda.Compiler.Backend
import Agda.Compiler.Common
import Agda.Interaction.FindFile
import Agda.Interaction.Imports
import Agda.Interaction.Library
import Agda.Interaction.Options
import Agda.Syntax.Common.Pretty qualified as P
import Agda.TypeChecking.Pretty
import Agda.Utils.FileName
import Agda.Utils.IO.Directory
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Maybe (ifJustM)
import Control.Foldl qualified as Foldl
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Yaml
import Paths_aegle
import System.Directory
import System.FilePath.Find qualified as Find

--------------------------------------------------------------------------------
-- Config

newtype Config = Config
  { libraryConfigs :: [LibraryConfig]
  }

data LibraryConfig = LibraryConfig
  { -- | Set of fully-qualified definition names subject to definition unfolding during search.
    transparentDefs :: S.Set TransparentDefName,
    -- | Path to Agda library.
    path :: FilePath
  }

data TransparentDefName = TransparentDefName
  { modName :: T.Text,
    name :: T.Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON TransparentDefName where
  parseJSON = withObject "TransparentDefName" \o ->
    TransparentDefName
      <$> (o .: "module")
      <*> (o .: "name")

--------------------------------------------------------------------------------

-- TODO: make indexer an Agda backend when Agda supports --build-library for arbitrary backend

-- Entrypoint
index :: Config -> TS.DbBuilder TCM a -> IO a
index Config {..} builder = runTCMTop' do
  primLibConfig <- liftIO loadPrimLibConfig
  Foldl.foldM
    (builder `inStagesM` flip indexOne)
    (primLibConfig : libraryConfigs)

loadPrimLibConfig :: IO LibraryConfig
loadPrimLibConfig = do
  path <- filePath <$> getPrimitiveLibDir
  transparentDefsFile <- getDataFileName "data/prim_transparent_defs.yaml"
  transparentDefs <- decodeFileThrow transparentDefsFile
  pure LibraryConfig {..}

indexOne :: LibraryConfig -> TS.DbBuilder TCM a -> TCM a
indexOne config builder = do
  resetAllState -- is this necessary?
  liftIO $ setCurrentDirectory config.path
  (Right (_, opts), _) <- pure $ runOptM $ parseBackendOptions [] [] defaultOptions
  setCommandLineOptions =<< addTrustedExecutables opts
  AgdaLibFile {_libIncludes = paths, _libPragmas = libOpts} <-
    libToTCM (getAgdaLibFile config.path) >>= \case
      [file] -> pure file
      [] -> aegleError "No libraries found to index"
      _ -> __IMPOSSIBLE__
  checkAndSetOptionsFromPragma libOpts
  importPrimitiveModules

  files <-
    liftIO $ sort . map Find.infoPath <$> do
      foldMap (findWithInfo (pure True) (hasAgdaExtension <$> Find.filePath)) paths

  -- Files are parsed twice, but this lowers peak memory residency

  transparentDefs <- resolveTransparentDefs config.transparentDefs files

  buildDb transparentDefs files builder

--------------------------------------------------------------------------------
-- Name resolution

resolveTransparentDefs :: S.Set TransparentDefName -> [FilePath] -> TCM (S.Set QName)
resolveTransparentDefs (S.null -> True) _ = pure mempty
resolveTransparentDefs transparentDefs files = do
  let grouped =
        M.fromSet (S.singleton . (.name)) transparentDefs
          & M.mapKeysWith (<>) (.modName)

  let step (!resolved, !unvisited) file = do
        src <- parseFile file
        let modName = T.show $ P.pretty src.srcModuleName
            names = M.lookup modName grouped
        resolved' <- foldMap (resolveNamesIn src) names
        pure $! resolved <> resolved' // S.delete modName unvisited

  (resolved, unvisited) <- foldM step (mempty, M.keysSet grouped) files

  unless (S.null unvisited) do
    aegleError $ "Could not find modules " <> pretty unvisited

  pure resolved

resolveNamesIn :: Source -> S.Set T.Text -> TCM (S.Set QName)
resolveNamesIn src names = do
  let modName = src.srcModuleName
  withCurrentModule noModuleName do
    withTopLevelModule modName do
      modInfo <- getNonMainModuleInfo modName (Just src)
      setInterface modInfo.miInterface
      withScope_ modInfo.miInterface.iInsideScope do
        S.fromList <$> for (S.toList names) \name ->
          resolveDefinedName (T.unpack name) >>= \case
            Just resolved -> pure resolved
            Nothing -> aegleError do
              "Could not find definition " <> pretty modName <> "." <> pretty name

--------------------------------------------------------------------------------
-- Indexing

buildDb :: S.Set QName -> [FilePath] -> TS.DbBuilder TCM a -> TCM a
buildDb transparentDefs files builder =
  flip Foldl.foldM files
    $ Foldl.premapM (parseFile >=> extractFragment transparentDefs) builder

extractFragment :: S.Set QName -> Source -> TCM TS.LibraryFragment
extractFragment transparentDefs src = do
  let modName = src.srcModuleName
  withCurrentModule noModuleName do
    withTopLevelModule modName do
      modInfo <- getNonMainModuleInfo modName (Just src)
      setInterface modInfo.miInterface
      -- skip cubical for now
      ifJustM (useTC (stPragmaOptions . lensOptCubical)) (\_ -> pure mempty) do
        runTransl Transl.Config {..} do
          translateScope modInfo.miInterface.iInsideScope

--------------------------------------------------------------------------------

parseFile :: FilePath -> TCM Source
parseFile file = do
  path <- liftIO $ absolute file
  sf <- srcFromPath path
  parseSource sf

inStagesM ::
  (Applicative m) =>
  Foldl.FoldM m a r ->
  (forall x. Foldl.FoldM m a x -> b -> m x) ->
  Foldl.FoldM m b r
Foldl.FoldM step begin done `inStagesM` runStage =
  Foldl.FoldM (\x -> runStage (Foldl.FoldM step (pure x) pure)) begin done
