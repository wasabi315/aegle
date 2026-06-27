module Aegle.Index.Translate.Scope
  ( translateScope,
    collectPublicNames,
  )
where

import Aegle.Database.Backend qualified as TS
import Aegle.Index.Translate
import Aegle.Index.Translate.Definition
import Aegle.Index.Translate.Name
import Aegle.Index.Utils
import Aegle.Prelude
import Agda.Compiler.Backend
import Agda.Syntax.Concrete.Name qualified as C
import Agda.Syntax.Scope.Base
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S

--------------------------------------------------------------------------------

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
