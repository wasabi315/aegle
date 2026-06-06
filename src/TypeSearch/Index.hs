{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TypeSearch.Index
  ( indexLibrary,
    Config (..),
  )
where

import Agda.Compiler.Backend
import Agda.Compiler.Common
import Agda.Interaction.FindFile
import Agda.Interaction.Imports
import Agda.Interaction.Library
import Agda.Interaction.Options
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.TypeChecking.Pretty
import Agda.Utils.FileName
import Agda.Utils.IO.Directory
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Maybe (ifJustM)
import Agda.Utils.Monad hiding (guard, unless)
import Control.Foldl qualified as Foldl
import Data.Set qualified as S
import Prettyprinter qualified as P
import System.Directory
import System.FilePath.Find qualified as Find
import System.IO
import TypeSearch.Core.Name qualified as TS
import TypeSearch.Database.Backend qualified as TS
import TypeSearch.Index.Translate (runTransl)
import TypeSearch.Index.Translate qualified as Transl
import TypeSearch.Index.Translate.Scope
import TypeSearch.Index.Utils
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Entrypoint

data Config = Config
  { -- | Set of fully-qualified definition names subject to definition unfolding during search.
    transparentDefNames :: S.Set TS.QName,
    -- | Path to Agda library.
    libraryDir :: FilePath
  }

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

    srcs <- for files \file -> do
      path <- liftIO $ absolute file
      sf <- srcFromPath path
      parseSource sf

    -- resolve transparent definition names
    transparentDefNames <- resolveQNamesIn srcs config.transparentDefNames

    flip Foldl.foldM srcs
      $ Foldl.premapM (extractFragment transparentDefNames)
      $ Foldl.hoists liftIO builder

resolveQNamesIn :: [Source] -> S.Set TS.QName -> TCM (S.Set QName)
resolveQNamesIn srcs transparentDefNames = do
  (transparentDefNames, unresolved) <-
    foldM
      ( \(resolved, unresolved) src -> do
          let m = src.srcModuleName
          withCurrentModule noModuleName do
            withTopLevelModule m do
              mi <- getNonMainModuleInfo m (Just src)
              setInterface mi.miInterface
              withScope_ mi.miInterface.iInsideScope do
                let mname = prettyShow mi.miInterface.iTopLevelModuleName
                    (toResolve, unresolved') = partition ((mname ==) . show . P.pretty . (.moduleName)) unresolved
                resolved' <- forM toResolve \x ->
                  resolveDefinedName (show $ P.pretty x.name) >>= \case
                    Nothing -> indexError $ "Couldn't find definition " <> pshow (P.pretty x)
                    Just x -> pure x
                pure (foldr S.insert resolved resolved', unresolved')
      )
      (mempty, S.toList transparentDefNames)
      srcs

  unless (null unresolved) do
    indexError
      $ vsep ["Couldn't find definitions", prettyList_ (map (pshow . P.pretty) unresolved)]

  pure transparentDefNames

extractFragment :: S.Set QName -> Source -> TCM TS.LibraryFragment
extractFragment transparentDefNames src = do
  let m = src.srcModuleName
  withCurrentModule noModuleName do
    withTopLevelModule m do
      mi <- getNonMainModuleInfo m (Just src)
      setInterface mi.miInterface
      -- skip cubical for now
      ifJustM (useTC (stPragmaOptions . lensOptCubical)) (\_ -> pure mempty) do
        runTransl Transl.Config {..} do
          translateScope mi.miInterface.iInsideScope
