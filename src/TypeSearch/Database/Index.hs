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
import ListT qualified
import System.Directory
import System.FilePath.Find qualified as Find
import System.IO
import TypeSearch.AgdaUtils
import TypeSearch.Core.Name qualified as TS
import TypeSearch.Database.Backend qualified as TS
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
          withCurrentModule noModuleName
            $ withTopLevelModule m do
              mi <- getNonMainModuleInfo m (Just src)
              setInterface mi.miInterface
              withScope_ mi.miInterface.iInsideScope do
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
  reexports <- lift $ collectReexportNames scopeInfo
  let reexports' =
        reexports
          <&> \(cname, aname) -> do
            let exportAs =
                  translateConcreteQName
                    (translateModuleName scopeInfo._scopeCurrent)
                    cname
                canonicalName = translateQName aname
            TS.Export {..}
      exports =
        fmap (\def -> TS.Export {canonicalName = def.name, exportAs = def.name}) definitions
          ++ reexports'
  pure $! TS.LibraryFragment {..}

isNameOfTypedThing :: AbstractName -> Bool
isNameOfTypedThing aname = aname.anameKind /= PatternSynName

collectPublicNames :: ScopeInfo -> S.Set QName
collectPublicNames scopeInfo =
  S.fromList $ DL.toList $ go $ getScope scopeInfo._scopeCurrent
  where
    getScope m = scopeInfo._scopeModules M.! m

    go scope = names <> namesInChildren
      where
        names = do
          anames <-
            DL.fromList
              $ M.elems
              $ namesInScope @AbstractName [PublicNS] scope
          aname <- DL.fromList $ NE.toList anames
          guard $ isNameOfTypedThing aname
          pure aname.anameName

        namesInChildren = do
          amods <-
            DL.fromList
              $ M.elems
              $ namesInScope @AbstractModule [PublicNS] scope
          amod <- DL.fromList $ NE.toList amods
          go (getScope amod.amodName)

collectReexportNames :: ScopeInfo -> TCM [(C.QName, QName)]
collectReexportNames scopeInfo = ListT.toList do
  let scope = scopeInfo._scopeModules M.! scopeInfo._scopeCurrent
  guard $ scope.scopeDatatypeModule /= Just IsDataModule
  (cname, anames) <- choose $ M.toList $ namesInScope @AbstractName [ImportedNS] scope
  aname <- choose anames
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
  pure $! TS.constructDefinition name' signature Nothing

translateFunDef :: Definition -> Transl TS.Definition
translateFunDef def = do
  let name = translateQName def.defName
  signature <- translateType def.defType
  body <- ifM
    (isTransparentDef def.defName)
    do Just <$> translateTransparentDefBody def
    do pure Nothing
  pure $! TS.constructDefinition name signature body
