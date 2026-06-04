{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TypeSearch.Database.Index
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
import Agda.Syntax.Concrete.Name qualified as C
import Agda.Syntax.Scope.Base
import Agda.Utils.FileName
import Agda.Utils.IO.Directory
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Maybe (ifJustM)
import Agda.Utils.Monad hiding (guard, unless)
import Control.Foldl qualified as Foldl
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Prettyprinter
import System.Directory
import System.FilePath.Find qualified as Find
import System.IO
import TypeSearch.AgdaUtils
import TypeSearch.Core.Evaluation qualified as TS
import TypeSearch.Core.Isomorphism qualified as TS
import TypeSearch.Core.Name qualified as TS
import TypeSearch.Core.Term qualified as TS
import TypeSearch.Database.Backend qualified as TS
import TypeSearch.Database.Feature qualified as TS
import TypeSearch.Prelude
import TypeSearch.Translate.Monad
import TypeSearch.Translate.Name
import TypeSearch.Translate.Term
import TypeSearch.Translate.TransparentDef

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
      $ Foldl.premapM (translateSource transparentDefNames)
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
                    (toResolve, unresolved') = partition ((mname ==) . show . pretty . (.moduleName)) unresolved
                resolved' <- forM toResolve \x ->
                  resolveDefinedName (show $ pretty x.name) >>= \case
                    Nothing -> throwError $ GenericException $ "Couldn't find definition " ++ show (pretty x)
                    Just x -> pure x
                pure (foldr S.insert resolved resolved', unresolved')
      )
      (mempty, S.toList transparentDefNames)
      srcs

  unless (null unresolved) do
    throwError $ GenericException $ "Couldn't find definitions " ++ show (prettyList unresolved)

  pure transparentDefNames

translateSource :: S.Set QName -> Source -> TCM TS.LibraryFragment
translateSource transparentDefNames src = do
  let m = src.srcModuleName
  withCurrentModule noModuleName do
    withTopLevelModule m do
      mi <- getNonMainModuleInfo m (Just src)
      setInterface mi.miInterface
      flip runTransl transparentDefNames do
        translateInterface mi.miInterface

--------------------------------------------------------------------------------

translateInterface :: Interface -> Transl TS.LibraryFragment
translateInterface intf =
  ifJustM (useTC (stPragmaOptions . lensOptCubical)) (\_ -> pure mempty) do
    translateScope intf.iInsideScope

translateScope :: ScopeInfo -> Transl TS.LibraryFragment
translateScope scopeInfo = do
  let pubNames = collectPublicNames scopeInfo
  definitions <- forMaybe (S.toList pubNames) \aname -> do
    def <- getConstInfo aname
    translateDefinition def
  let reexports =
        collectReexportNames scopeInfo
          <&> \(cname, aname) -> do
            let exportAs =
                  translateConcreteQName
                    (translateModuleName scopeInfo._scopeCurrent)
                    cname
                canonicalName = translateQName aname
            TS.Export {..}
      exports =
        fmap (\def -> TS.Export {canonicalName = def.name, exportAs = def.name}) definitions
          ++ reexports
  pure $! TS.LibraryFragment {..}

isNameOfTypedThing :: AbstractName -> Bool
isNameOfTypedThing aname = aname.anameKind /= PatternSynName

collectPublicNames :: ScopeInfo -> S.Set QName
collectPublicNames scopeInfo =
  S.fromList $ go $ getScope scopeInfo._scopeCurrent
  where
    getScope m = scopeInfo._scopeModules M.! m

    go scope = names ++ namesInChildren
      where
        names = do
          anames <- M.elems $ namesInScope @AbstractName [PublicNS] scope
          aname <- NE.toList anames
          guard $ isNameOfTypedThing aname
          pure aname.anameName

        namesInChildren = do
          amods <- M.elems $ namesInScope @AbstractModule [PublicNS] scope
          amod <- NE.toList amods
          go (getScope amod.amodName)

collectReexportNames :: ScopeInfo -> [(C.QName, QName)]
collectReexportNames scopeInfo = do
  let scope = scopeInfo._scopeModules M.! scopeInfo._scopeCurrent
  (cname, anames) <- M.toList $ namesInScope @AbstractName [ImportedNS] scope
  aname <- NE.toList anames
  guard $ isNameOfTypedThing aname
  pure (C.QName cname, useCanonical aname.anameName)

--------------------------------------------------------------------------------

translateDefinition :: Definition -> Transl (Maybe TS.Definition)
translateDefinition def = setCurrentRangeQ def.defName do
  ifM (isErasable def.defType) (pure Nothing) case def.theDef of
    AxiomDefn {} -> Just <$> translateToAxiom def
    AbstractDefn {} -> Just <$> translateToAxiom def
    FunctionDefn {} -> Just <$> translateFunDef def
    DatatypeDefn {} -> Just <$> translateToAxiom def
    RecordDefn {} -> Just <$> translateToAxiom def
    ConstructorDefn {} -> Just <$> translateToAxiom def
    PrimitiveDefn {} -> Just <$> translateToAxiom def
    DataOrRecSigDefn {} -> pure Nothing
    GeneralizableVar {} -> pure Nothing
    PrimitiveSortDefn {} -> pure Nothing

translateToAxiom :: Definition -> Transl TS.Definition
translateToAxiom def = do
  let name' = translateQName def.defName
  signature <- translateType def.defType
  pure $! constructDefinition name' signature Nothing

translateFunDef :: Definition -> Transl TS.Definition
translateFunDef def = do
  let name = translateQName def.defName
  signature <- translateType def.defType
  body <- ifM
    (isTransparentDef def.defName)
    do Just <$> translateTransparentDefBody def
    do pure Nothing
  pure $! constructDefinition name signature body

constructDefinition :: TS.QName -> TS.Type -> Maybe TS.Term -> TS.Definition
constructDefinition name signature body = TS.Definition {..}
  where
    (signature', _) = TS.normalise0 (TS.emptyMetaCtx mempty) mempty signature
    feature = TS.allFeature signature'
