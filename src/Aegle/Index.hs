{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Aegle.Index
  ( indexLibrary,
    Config (..),
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
import System.Directory
import System.FilePath.Find qualified as Find

--------------------------------------------------------------------------------
-- Entrypoint

data Config = Config
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

-- TODO: make indexer an Agda backend when Agda supports --build-library for arbitrary backend

indexLibrary :: Config -> TS.DbBuilder IO a -> IO a
indexLibrary config builder = do
  setCurrentDirectory config.libraryDir
  (Right (_, opts), _) <- pure $ runOptM $ parseBackendOptions [] [] defaultOptions
  runTCMTop' do
    opts <- addTrustedExecutables opts
    setCommandLineOptions opts
    cwd <- liftIO getCurrentDirectory
    ls <- libToTCM $ getAgdaLibFile cwd
    AgdaLibFile
      { _libIncludes = paths,
        _libPragmas = libOpts
      } <- case ls of
      [l] -> pure l
      [] -> throwError $ GenericException "No library found to build"
      _ -> __IMPOSSIBLE__
    checkAndSetOptionsFromPragma libOpts
    importPrimitiveModules

    libDirPrim <- useTC stPrimitiveLibDir
    files <-
      liftIO
        $ sort
        . map Find.infoPath
        . concat
        <$> forM (filePath libDirPrim : paths) (findWithInfo (pure True) (hasAgdaExtension <$> Find.filePath))

    srcs <-
      M.fromDistinctAscList <$> for files \file -> do
        path <- liftIO $ absolute file
        sf <- srcFromPath path
        src <- parseSource sf
        let modName = T.show $ P.pretty src.srcModuleName
        pure (modName, src)

    transparentDefNames <- resolveTransparentDefNames srcs config.transparentDefNames

    flip Foldl.foldM srcs
      $ Foldl.premapM (extractFragment transparentDefNames)
      $ Foldl.hoists liftIO builder

resolveTransparentDefNames ::
  M.Map T.Text Source -> S.Set TransparentDefName -> TCM (S.Set QName)
resolveTransparentDefNames srcs transparentDefNames = do
  let grouped =
        S.toAscList transparentDefNames
          & map (\TransparentDefName {..} -> (toplevelModuleName, [name]))
          & M.fromAscListWith (++)
  flip M.foldMapWithKey grouped \modName names -> do
    src <- case M.lookup modName srcs of
      Just src -> pure src
      Nothing -> indexError $ "Could not find module " <> pretty modName
    resolved <- resolveNamesIn src names
    pure $! S.fromList resolved

resolveNamesIn :: (Traversable t) => Source -> t T.Text -> TCM (t QName)
resolveNamesIn src names = do
  let modName = src.srcModuleName
  withCurrentModule noModuleName do
    withTopLevelModule modName do
      modInfo <- getNonMainModuleInfo modName (Just src)
      setInterface modInfo.miInterface
      withScope_ modInfo.miInterface.iInsideScope do
        for names \name ->
          resolveDefinedName (T.unpack name) >>= \case
            Just resolved -> pure resolved
            Nothing -> indexError do
              "Could not find definition " <> pretty modName <> "." <> pretty name

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
