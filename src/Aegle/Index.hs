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
import Data.Aeson (eitherDecodeFileStrict)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
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
    transparentDefNames :: S.Set TransparentDefName,
    -- | Path to Agda library.
    libraryDir :: FilePath
  }

data TransparentDefName = TransparentDefName
  { toplevelModuleName :: T.Text,
    name :: T.Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON)

--------------------------------------------------------------------------------

-- TODO: make indexer an Agda backend when Agda supports --build-library for arbitrary backend

-- Entrypoint
index :: Config -> TS.DbBuilder IO a -> IO a
index Config {..} builder = do
  primLibConfig <- loadPrimLibConfig
  Foldl.foldM
    (builder `inStagesM` flip indexOne)
    (primLibConfig : libraryConfigs)

loadPrimLibConfig :: IO LibraryConfig
loadPrimLibConfig = do
  libraryDir <- filePath <$> getPrimitiveLibDir
  transparentDefsFile <- getDataFileName "data/prim_transparent_defs.json"
  transparentDefNames <-
    eitherDecodeFileStrict transparentDefsFile
      <&> either
        (\e -> error $ "Failed to load " ++ transparentDefsFile ++ ": " ++ e)
        id
  pure LibraryConfig {..}

indexOne :: LibraryConfig -> TS.DbBuilder IO a -> IO a
indexOne config builder = withCurrentDirectory config.libraryDir do
  (Right (_, opts), _) <- pure $ runOptM $ parseBackendOptions [] [] defaultOptions
  runTCMTop' do
    opts <- addTrustedExecutables opts
    setCommandLineOptions opts
    cwd <- liftIO getCurrentDirectory
    ls <- libToTCM $ getAgdaLibFile cwd
    AgdaLibFile {_libIncludes = paths, _libPragmas = libOpts} <- case ls of
      [l] -> pure l
      [] -> throwError $ GenericException "No library found to build"
      _ -> __IMPOSSIBLE__
    checkAndSetOptionsFromPragma libOpts
    importPrimitiveModules

    files <-
      liftIO
        $ sort
        . map Find.infoPath
        . concat
        <$> forM paths (findWithInfo (pure True) (hasAgdaExtension <$> Find.filePath))

    -- Files are parsed twice, but 30% lower memory residency

    transparentDefNames <- resolveTransparentDefNames config.transparentDefNames files

    buildDb transparentDefNames files builder

--------------------------------------------------------------------------------
-- Name resolution

resolveTransparentDefNames :: S.Set TransparentDefName -> [FilePath] -> TCM (S.Set QName)
resolveTransparentDefNames (S.null -> True) _ = pure mempty
resolveTransparentDefNames transparentDefNames files = do
  let grouped =
        S.toAscList transparentDefNames
          & map (\TransparentDefName {..} -> (toplevelModuleName, S.singleton name))
          & M.fromAscListWith (<>)

  let step (!resolved, !unvisited) file = do
        src <- parseFile file
        let modName = T.show $ P.pretty src.srcModuleName
        resolved <- (resolved <>) <$> resolveNames grouped src
        unvisited <- pure $ S.delete modName unvisited
        pure (resolved, unvisited)

  (resolved, unvisited) <- foldM step (mempty, M.keysSet grouped) files

  unless (S.null unvisited) do
    aegleError $ "Could not find modules " <> pretty unvisited

  pure resolved

resolveNames :: M.Map T.Text (S.Set T.Text) -> Source -> TCM (S.Set QName)
resolveNames grouped src = do
  let modName = src.srcModuleName
      names = M.lookup (T.show $ P.pretty modName) grouped
  flip foldMap names \names -> withCurrentModule noModuleName do
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

buildDb :: S.Set QName -> [FilePath] -> TS.DbBuilder IO a -> TCM a
buildDb transparentDefNames files builder =
  flip Foldl.foldM files
    $ Foldl.premapM (parseFile >=> extractFragment transparentDefNames)
    $ Foldl.hoists liftIO builder

extractFragment :: S.Set QName -> Source -> TCM TS.LibraryFragment
extractFragment transparentDefNames src = do
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
