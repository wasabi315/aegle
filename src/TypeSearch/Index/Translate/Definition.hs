{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TypeSearch.Index.Translate.Definition
  ( translateDefinition,
  )
where

import Agda.Compiler.Backend
import Agda.Utils.Monad hiding (guard, unless)
import TypeSearch.Core.Evaluation qualified as TS
import TypeSearch.Core.Isomorphism qualified as TS
import TypeSearch.Core.Name qualified as TS
import TypeSearch.Core.Term qualified as TS
import TypeSearch.Database.Backend qualified as TS
import TypeSearch.Database.Feature qualified as TS
import TypeSearch.Index.Translate
import TypeSearch.Index.Translate.Name
import TypeSearch.Index.Translate.Term
import TypeSearch.Index.Translate.TransparentDef
import TypeSearch.Index.Utils
import TypeSearch.Prelude

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
