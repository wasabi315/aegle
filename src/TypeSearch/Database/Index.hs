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
import Agda.Syntax.Internal
import Agda.Syntax.Position
import Agda.Syntax.Scope.Base
import Agda.Utils.FileName
import Agda.Utils.IO.Directory
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Maybe (ifJustM)
import Agda.Utils.Monad hiding (guard, unless)
import Data.DList qualified as DL
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
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
    libraryDir :: FilePath,
    -- | DB builder backend.
    dbBuilder :: TS.DbBuilder TCM
  }

-- TODO: make indexer an Agda backend

indexLibrary :: Config -> IO ()
indexLibrary config = do
  -- TS.migrate config.dbConn
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

    -- resolve transparent definition names
    -- FIXME: make this more sensible. traversing entire library twice currently.
    (transparentDefNames, unresolved) <-
      foldM
        ( \(resolved, unresolved) inputFile -> do
            path <- liftIO (absolute inputFile)
            sf <- srcFromPath path
            src <- parseSource sf
            let m = srcModuleName src
            setCurrentRange (beginningOfFile path) do
              checkModuleName m (srcOrigin src) Nothing
              withCurrentModule noModuleName
                $ withTopLevelModule m
                $ do
                  mi <- getNonMainModuleInfo m (Just src)
                  setInterface mi.miInterface
                  withScope_ mi.miInterface.iInsideScope do
                    let mname = prettyShow mi.miInterface.iTopLevelModuleName
                        (toResolve, unresolved') = partition ((mname ==) . show . (.moduleName)) unresolved
                    resolved' <- forM toResolve \x ->
                      resolveDefinedName (show x.name) >>= \case
                        Nothing -> throwError $ GenericException $ "Couldn't find definition " ++ show x
                        Just x -> pure x
                    pure (foldr S.insert resolved resolved', unresolved')
        )
        (mempty, S.toList config.transparentDefNames)
        files

    unless (null unresolved) do
      throwError $ GenericException $ "Couldn't find definitions " ++ show unresolved

    config.dbBuilder.build do
      inputFile <- choose files
      lift do
        path <- liftIO (absolute inputFile)
        sf <- srcFromPath path
        src <- parseSource sf
        let m = srcModuleName src
        setCurrentRange (beginningOfFile path) do
          checkModuleName m (srcOrigin src) Nothing
          withCurrentModule noModuleName
            $ withTopLevelModule m do
              mi <- getNonMainModuleInfo m (Just src)
              setInterface mi.miInterface
              withScope_ mi.miInterface.iInsideScope do
                runTransl transparentDefNames do
                  translateInterface mi.miInterface

--------------------------------------------------------------------------------

translateInterface :: Interface -> Transl TS.LibraryFragment
translateInterface intf =
  ifJustM (useTC (stPragmaOptions . lensOptCubical)) (\_ -> pure mempty) do
    translateScope intf.iInsideScope

translateScope :: ScopeInfo -> Transl TS.LibraryFragment
translateScope scopeInfo = do
  definitions <- forMaybe (collectPublicNames scopeInfo) \(cname, aname) -> do
    def <- getConstInfo aname
    let name =
          translateConcreteQName
            (translateModuleName scopeInfo._scopeCurrent)
            cname
    translateDefinition name def
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

collectPublicNames :: ScopeInfo -> [(C.QName, QName)]
collectPublicNames scopeInfo =
  DL.toList $ go id $ getScope scopeInfo._scopeCurrent
  where
    getScope m = scopeInfo._scopeModules M.! m

    go _ scope
      | scope.scopeDatatypeModule == Just IsDataModule = mempty
    go qual scope = names <> namesInChildren
      where
        names = do
          (cname, anames) <-
            DL.fromList
              $ M.toList
              $ namesInScope @AbstractName [PublicNS] scope
          aname <- DL.fromList $ NE.toList anames
          guard $ aname.anameKind /= PatternSynName
          pure (qual (C.QName cname), aname.anameName)

        namesInChildren = do
          (cmodName, amods) <-
            DL.fromList
              $ M.toList
              $ namesInScope @AbstractModule [PublicNS] scope
          amod <- DL.fromList $ NE.toList amods
          go (qual . C.Qual cmodName) (getScope amod.amodName)

collectReexportNames :: ScopeInfo -> [(C.QName, QName)]
collectReexportNames scopeInfo = do
  let scope = scopeInfo._scopeModules M.! scopeInfo._scopeCurrent
  guard $ scope.scopeDatatypeModule /= Just IsDataModule
  (cname, anames) <- M.toList $ namesInScope @AbstractName [ImportedNS] scope
  aname <- NE.toList anames
  guard $ aname.anameKind /= PatternSynName
  pure (C.QName cname, useCanonical aname.anameName)

--------------------------------------------------------------------------------

translateDefinition :: TS.QName -> Definition -> Transl (Maybe TS.Definition)
translateDefinition qname def = setCurrentRangeQ def.defName do
  ifM
    (orM [isErasable def.defType, isDeprecated def.defName])
    do pure Nothing
    case def.theDef of
      AxiomDefn {} -> Just <$> translateToAxiom qname def.defType
      AbstractDefn {} -> Just <$> translateToAxiom qname def.defType
      FunctionDefn {} -> Just <$> translateFunDef qname def
      DatatypeDefn {} -> Just <$> translateToAxiom qname def.defType
      RecordDefn {} -> Just <$> translateToAxiom qname def.defType
      ConstructorDefn {} -> Just <$> translateToAxiom qname def.defType
      PrimitiveDefn {} -> Just <$> translateToAxiom qname def.defType
      DataOrRecSigDefn {} -> pure Nothing
      GeneralizableVar {} -> pure Nothing
      PrimitiveSortDefn {} -> pure Nothing

translateToAxiom :: TS.QName -> Type -> Transl TS.Definition
translateToAxiom name sig = do
  signature <- locallyReduceTransparentDef $ translateType sig
  pure $! constructDefinition name signature Nothing

translateFunDef :: TS.QName -> Definition -> Transl TS.Definition
translateFunDef name def = do
  signature <- locallyReduceTransparentDef $ translateType def.defType
  body <- ifM
    (isTransparentDef def.defName)
    do Just <$> translateTransparentDefBody def
    do pure Nothing
  pure $! constructDefinition name signature body

constructDefinition :: TS.QName -> TS.Type -> Maybe TS.Term -> TS.Definition
constructDefinition name signature body = TS.Definition {..}
  where
    (signature', _) = TS.normalise0 TS.emptyMetaCtx mempty signature
    feature = TS.feature signature'
