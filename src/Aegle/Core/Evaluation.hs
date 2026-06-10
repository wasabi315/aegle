module Aegle.Core.Evaluation where

import Aegle.Core.Name
import Aegle.Core.Term
import Aegle.Prelude
import Data.IntMap.Strict qualified as IM
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set.NonEmpty qualified as S1
import Data.Set.NonEmpty.Extra qualified as S1
import Prettyprinter

--------------------------------------------------------------------------------
-- Values

-- | Values
data Value
  = VRigid Level Spine
  | VFlex MetaVar Spine
  | VTop {-# UNPACK #-} QName Spine
  | VTopAmb PQName Spine
  | VU
  | VPi Name VType (Value -> VType)
  | VLam Name (Value -> Value)
  | VSigma Name VType (Value -> VType)
  | VPair Value Value
  | VBrave Value Spine

type VType = Value

data Spine
  = SNil
  | SApp Spine Value
  | SProj1 Spine
  | SProj2 Spine

pattern VVar :: Level -> Value
pattern VVar x = VRigid x SNil

pattern VMeta :: MetaVar -> Value
pattern VMeta m = VFlex m SNil

data Quant = Quant Name Value (Value -> Value)

-- | Environment keyed by De Bruijn indices
type Env = [Value]

-- | Environment keyed by top-level names
type TopEnv = ML.Map QName Value

-- | Meta-context
data MetaCtx = MetaCtx
  { nextMeta :: MetaVar,
    metaCtx :: IM.IntMap MetaEntry,
    resol :: Resol
  }

data MetaEntry
  = Unsolved ~Value
  | Solved Value ~Value

-- | Unresolved name → set of canonical names denoted
type Resol = M.Map PQName (S1.NESet QName)

emptyMetaCtx :: Resol -> MetaCtx
emptyMetaCtx = MetaCtx 0 mempty

allMetaSolved :: MetaCtx -> Bool
allMetaSolved mctx = flip all mctx.metaCtx \case
  Unsolved {} -> False
  Solved {} -> True

lookupResol :: MetaCtx -> PQName -> S1.NESet QName
lookupResol mctx n = mctx.resol M.! n

resolve :: MetaCtx -> PQName -> QName -> MetaCtx
resolve mctx m n = mctx {resol = M.insert m (S1.singleton n) mctx.resol}

--------------------------------------------------------------------------------
-- Evaluation

eval :: MetaCtx -> TopEnv -> Env -> Term -> Value
eval mctx tenv env = \case
  Var (Index x) -> env !! x
  Meta m -> vMeta mctx m
  Top x -> vTop tenv x
  TopAmb x -> vTopAmb mctx tenv x
  U -> VU
  Pi x a b -> VPi x (eval mctx tenv env a) (evalBind mctx tenv env b)
  Lam x t -> VLam x (evalBind' mctx tenv env t)
  App t u -> eval mctx tenv env t $$ eval mctx tenv env u
  Sigma x a b -> VSigma x (eval mctx tenv env a) (evalBind mctx tenv env b)
  Pair t u -> VPair (eval mctx tenv env t) (eval mctx tenv env u)
  Proj1 t -> vProj1 (eval mctx tenv env t)
  Proj2 t -> vProj2 (eval mctx tenv env t)
  AppPruning t pr -> vAppPruning env (eval mctx tenv env t) pr

evalBind :: MetaCtx -> TopEnv -> Env -> Term -> (Value -> Value)
evalBind mctx tenv env t ~u = eval mctx tenv (u : env) t

evalBind' :: MetaCtx -> TopEnv -> Env -> Term -> (Value -> Value)
evalBind' mctx tenv env t u = eval mctx tenv (u : env) t

vMeta :: MetaCtx -> MetaVar -> Value
vMeta mctx m = case mctx.metaCtx IM.! coerce m of
  Unsolved {} -> VMeta m
  Solved v _ -> v

vTop :: TopEnv -> QName -> Value
vTop tenv n = ML.findWithDefault (VTop n SNil) n tenv

-- Reduce only when the name has been resolved
vTopAmb :: MetaCtx -> TopEnv -> PQName -> Value
vTopAmb mctx tenv n = case mctx.resol M.! n of
  S1.Singleton n' -> vTop tenv n'
  _ -> VTopAmb n SNil

vAppPruning :: Env -> Value -> Pruning -> Value
vAppPruning env ~v pr = case (env, pr) of
  ([], []) -> v
  (t : env, True : pr) -> vAppPruning env v pr $$ t
  (_ : env, False : pr) -> vAppPruning env v pr
  _ -> impossible "vAppPruning"

($$) :: Value -> Value -> Value
t $$ u = case t of
  VLam _ t -> t u
  VRigid x sp -> VRigid x (SApp sp u)
  VFlex m sp -> VFlex m (SApp sp u)
  VTop x sp -> VTop x (SApp sp u)
  VTopAmb x sp -> VTopAmb x (SApp sp u)
  VBrave b sp -> VBrave b (SApp sp u)
  t -> VBrave t (SApp SNil u)

vProj1 :: Value -> Value
vProj1 = \case
  VPair t _ -> t
  VRigid x sp -> VRigid x (SProj1 sp)
  VFlex m sp -> VFlex m (SProj1 sp)
  VTop x sp -> VTop x (SProj1 sp)
  VTopAmb x sp -> VTopAmb x (SProj1 sp)
  VBrave b sp -> VBrave b (SProj1 sp)
  t -> VBrave t (SProj1 SNil)

vProj2 :: Value -> Value
vProj2 = \case
  VPair _ t -> t
  VRigid x sp -> VRigid x (SProj2 sp)
  VFlex m sp -> VFlex m (SProj2 sp)
  VTop x sp -> VTop x (SProj2 sp)
  VTopAmb x sp -> VTopAmb x (SProj2 sp)
  VBrave b sp -> VBrave b (SProj2 sp)
  t -> VBrave t (SProj2 SNil)

vAppSpine :: Value -> Spine -> Value
vAppSpine t = \case
  SNil -> t
  SApp sp u -> vAppSpine t sp $$ u
  SProj1 sp -> vProj1 $ vAppSpine t sp
  SProj2 sp -> vProj2 $ vAppSpine t sp

force :: MetaCtx -> TopEnv -> Value -> Value
force mctx tenv = \case
  VFlex m sp
    | Solved t _ <- mctx.metaCtx IM.! coerce m ->
        force mctx tenv (vAppSpine t sp)
  VTopAmb n sp
    | S1.Singleton n' <- mctx.resol M.! n ->
        force mctx tenv (vAppSpine (vTop tenv n') sp)
  t -> t

forceAmb :: MetaCtx -> TopEnv -> Value -> [(Value, MetaCtx)]
forceAmb mctx tenv = \case
  VFlex m sp
    | Solved t _ <- mctx.metaCtx IM.! coerce m ->
        forceAmb mctx tenv (vAppSpine t sp)
  VTopAmb n sp -> do
    n' <- toList $ mctx.resol M.! n
    let mctx' = resolve mctx n n'
    forceAmb mctx' tenv (vAppSpine (vTop tenv n') sp)
  t -> pure (t, mctx)

forceAmb' :: MetaCtx -> TopEnv -> Value -> [(Value, MetaCtx)]
forceAmb' mctx tenv = \case
  VFlex m sp
    | Solved t _ <- mctx.metaCtx IM.! coerce m ->
        forceAmb' mctx tenv (vAppSpine t sp)
  VTopAmb n sp ->
    (VTopAmb n sp, mctx) : do
      n' <- toList $ mctx.resol M.! n
      guard $ n' `ML.member` tenv
      let mctx' = resolve mctx n n'
      forceAmb' mctx' tenv (vAppSpine (vTop tenv n') sp)
  t -> pure (t, mctx)

--------------------------------------------------------------------------------
-- Quotation

levelToIndex :: Level -> Level -> Index
levelToIndex (Level l) (Level x) = Index (l - x - 1)

quote :: MetaCtx -> TopEnv -> Level -> Value -> Term
quote mctx tenv l t = case force mctx tenv t of
  VRigid x sp -> quoteSpine mctx tenv l (Var (levelToIndex l x)) sp
  VFlex m sp -> quoteSpine mctx tenv l (Meta m) sp
  VTop x sp -> quoteSpine mctx tenv l (Top x) sp
  VTopAmb x sp -> quoteSpine mctx tenv l (TopAmb x) sp
  VU -> U
  VPi x a b -> Pi x (quote mctx tenv l a) (quoteBind mctx tenv l b)
  VLam x t -> Lam x (quoteBind mctx tenv l t)
  VSigma x a b -> Sigma x (quote mctx tenv l a) (quoteBind mctx tenv l b)
  VPair t u -> Pair (quote mctx tenv l t) (quote mctx tenv l u)
  VBrave t sp -> quoteSpine mctx tenv l (quote mctx tenv l t) sp

quoteBind :: MetaCtx -> TopEnv -> Level -> (Value -> Value) -> Term
quoteBind mctx tenv l b = quote mctx tenv (l + 1) (b $ VVar l)

quoteSpine :: MetaCtx -> TopEnv -> Level -> Term -> Spine -> Term
quoteSpine mctx tenv l h = \case
  SNil -> h
  SApp sp u -> quoteSpine mctx tenv l h sp `App` quote mctx tenv l u
  SProj1 sp -> Proj1 $ quoteSpine mctx tenv l h sp
  SProj2 sp -> Proj2 $ quoteSpine mctx tenv l h sp

quoteAmb :: MetaCtx -> TopEnv -> Level -> Value -> [Term]
quoteAmb mctx tenv l t = do
  (t, mctx) <- forceAmb mctx tenv t
  case t of
    VRigid x sp -> quoteSpineAmb mctx tenv l (Var (levelToIndex l x)) sp
    VFlex m sp -> quoteSpineAmb mctx tenv l (Meta m) sp
    VTop x sp -> quoteSpineAmb mctx tenv l (Top x) sp
    VTopAmb x sp -> quoteSpineAmb mctx tenv l (TopAmb x) sp
    VU -> pure U
    VPi x a b ->
      Pi x
        <$> quoteAmb mctx tenv l a
        <*> quoteBindAmb mctx tenv l b
    VLam x t ->
      Lam x
        <$> quoteBindAmb mctx tenv l t
    VSigma x a b ->
      Sigma x
        <$> quoteAmb mctx tenv l a
        <*> quoteBindAmb mctx tenv l b
    VPair t u ->
      Pair
        <$> quoteAmb mctx tenv l t
        <*> quoteAmb mctx tenv l u
    VBrave {} -> []

quoteBindAmb :: MetaCtx -> TopEnv -> Level -> (Value -> Value) -> [Term]
quoteBindAmb mctx tenv l b = quoteAmb mctx tenv (l + 1) (b $ VVar l)

quoteSpineAmb :: MetaCtx -> TopEnv -> Level -> Term -> Spine -> [Term]
quoteSpineAmb mctx tenv l h = \case
  SNil -> pure h
  SApp sp u ->
    App
      <$> quoteSpineAmb mctx tenv l h sp
      <*> quoteAmb mctx tenv l u
  SProj1 sp -> Proj1 <$> quoteSpineAmb mctx tenv l h sp
  SProj2 sp -> Proj2 <$> quoteSpineAmb mctx tenv l h sp

--------------------------------------------------------------------------------
-- Prettyprinting

instance Pretty TopEnv where
  pretty tenv =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ [ pretty m
            <+> "="
            <+> pretty ((tenv, emptyMetaCtx mempty, Level 0) :⊢ t)
        | (m, t) <- ML.toList tenv
        ]

instance Pretty (TopEnv ⊢ MetaCtx) where
  pretty (tenv :⊢ mctx) =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ [ pretty (MetaVar m)
            <+> "="
            <+> maybe "?" (pretty . ((tenv, mctx, Level 0) :⊢)) sol
        | (m, entry) <- IM.toList mctx.metaCtx,
          let sol = case entry of
                Solved t ~_ -> Just t
                Unsolved ~_ -> Nothing
        ]
      ++ [ pretty x <+> case xs of
             S1.Singleton x' -> "=" <+> pretty x'
             _ -> "∈" <+> align (list (fmap pretty $ NE.toList $ S1.toList xs))
         | (x, xs) <- M.toList mctx.resol
         ]

instance Pretty ((TopEnv, MetaCtx, Level) ⊢ Value) where
  pretty ((tenv, mctx, lvl) :⊢ v) = pretty $ quote mctx tenv lvl v

instance Pretty ((TopEnv, MetaCtx, [Name]) ⊢ Value) where
  pretty ((tenv, mctx, ns) :⊢ v) = pretty (ns :⊢ quote mctx tenv (coerce $ length ns) v)
