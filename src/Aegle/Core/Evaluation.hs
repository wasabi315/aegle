module Aegle.Core.Evaluation
  ( module Aegle.Core.Evaluation,
  )
where

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
  = Unsolved ~VType
  | Solved Value ~VType

-- | Unresolved name → set of canonical names may denoted
type Resol = M.Map PQName (S1.NESet QName)

--------------------------------------------------------------------------------
-- Meta-context operation

emptyMetaCtx :: Resol -> MetaCtx
emptyMetaCtx = MetaCtx 0 mempty

newMeta :: MetaCtx -> VType -> (MetaVar, MetaCtx)
newMeta mctx ~mty = do
  let m' = mctx.nextMeta
      mctx' =
        mctx
          { nextMeta = mctx.nextMeta + 1,
            metaCtx = IM.insert (coerce m') (Unsolved mty) mctx.metaCtx
          }
  (m', mctx')

allMetaSolved :: MetaCtx -> Bool
allMetaSolved mctx = flip all mctx.metaCtx \case
  Unsolved {} -> False
  Solved {} -> True

lookupUnsolved :: MetaCtx -> MetaVar -> Value
lookupUnsolved mctx m = case mctx.metaCtx IM.! coerce m of
  Unsolved a -> a
  Solved {} -> error "lookupUnsolved"

writeMeta :: MetaCtx -> MetaVar -> Value -> VType -> MetaCtx
writeMeta mctx m t ~a =
  mctx {metaCtx = IM.insert (coerce m) (Solved t a) mctx.metaCtx}

lookupResol :: MetaCtx -> PQName -> S1.NESet QName
lookupResol mctx n = mctx.resol M.! n

-- | Resolve an unresolved name @n@ as a canonical name @n'@.
-- Returns 'Nothing' if @n@ does not denote @n'@ according to the given 'MetaCtx'.
resolve :: MetaCtx -> PQName -> QName -> Maybe MetaCtx
resolve mctx n n' = do
  guard $ n' `S1.member` (mctx.resol M.! n)
  pure $! unsafeResolve mctx n n'

unsafeResolve :: MetaCtx -> PQName -> QName -> MetaCtx
unsafeResolve mctx n n' = unsafeRestrict mctx n (S1.singleton n')

unsafeRestrict :: MetaCtx -> PQName -> S1.NESet QName -> MetaCtx
unsafeRestrict mctx n ns = mctx {resol = M.insert n ns mctx.resol}

--------------------------------------------------------------------------------
-- Evaluation

idEnv :: Level -> Env
idEnv l = VVar <$> (l - 1) `down` 0

eval :: TopEnv -> MetaCtx -> Env -> Term -> Value
eval tenv mctx env = \case
  Var (Index x) -> env !! x
  Meta m -> vMeta mctx m
  Top x -> vTop tenv x
  TopAmb x -> vTopAmb tenv mctx x
  U -> VU
  Pi x a b -> VPi x (eval tenv mctx env a) (evalBind tenv mctx env b)
  Lam x t -> VLam x (evalBind' tenv mctx env t)
  App t u -> eval tenv mctx env t $$ eval tenv mctx env u
  Sigma x a b -> VSigma x (eval tenv mctx env a) (evalBind tenv mctx env b)
  Pair t u -> VPair (eval tenv mctx env t) (eval tenv mctx env u)
  Proj1 t -> vProj1 (eval tenv mctx env t)
  Proj2 t -> vProj2 (eval tenv mctx env t)
  AppPruning t pr -> vAppPruning env (eval tenv mctx env t) pr

evalBind :: TopEnv -> MetaCtx -> Env -> Term -> (Value -> Value)
evalBind tenv mctx env t ~u = eval tenv mctx (u : env) t

evalBind' :: TopEnv -> MetaCtx -> Env -> Term -> (Value -> Value)
evalBind' tenv mctx env t u = eval tenv mctx (u : env) t

vMeta :: MetaCtx -> MetaVar -> Value
vMeta mctx m = case mctx.metaCtx IM.! coerce m of
  Unsolved {} -> VMeta m
  Solved v _ -> v

vTop :: TopEnv -> QName -> Value
vTop tenv n = ML.findWithDefault (VTop n SNil) n tenv

-- Reduce only when the name has been resolved
vTopAmb :: TopEnv -> MetaCtx -> PQName -> Value
vTopAmb tenv mctx n = case lookupResol mctx n of
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

force :: TopEnv -> MetaCtx -> Value -> Value
force tenv mctx = \case
  VFlex m sp
    | Solved t _ <- mctx.metaCtx IM.! coerce m ->
        force tenv mctx (vAppSpine t sp)
  VTopAmb n sp
    | S1.Singleton n' <- lookupResol mctx n ->
        force tenv mctx (vAppSpine (vTop tenv n') sp)
  t -> t

expandNondet :: TopEnv -> MetaCtx -> PQName -> Spine -> [(Value, MetaCtx)]
expandNondet tenv mctx n sp = do
  n' <- toList $ lookupResol mctx n
  Just t <- pure $ ML.lookup n' tenv
  let mctx' = unsafeResolve mctx n n'
      v = vAppSpine t sp
  pure (v, mctx')

forceNondet :: TopEnv -> MetaCtx -> Value -> [(Value, MetaCtx)]
forceNondet tenv mctx = \case
  VFlex m sp
    | Solved t _ <- mctx.metaCtx IM.! coerce m ->
        forceNondet tenv mctx (vAppSpine t sp)
  VTopAmb n sp
    | S1.Singleton n' <- lookupResol mctx n ->
        forceNondet tenv mctx (vAppSpine (vTop tenv n') sp)
  VTopAmb n sp -> do
    asum
      [ do
          ns <- S1.withNonEmpty empty pure do
            S1.filter (`ML.notMember` tenv) (lookupResol mctx n)
          let mctx' = unsafeRestrict mctx n ns
          pure (VTopAmb n sp, mctx'),
        expandNondet tenv mctx n sp
      ]
  t -> pure (t, mctx)

--------------------------------------------------------------------------------
-- Quotation

levelToIndex :: Level -> Level -> Index
levelToIndex (Level l) (Level x) = Index (l - x - 1)

quote :: TopEnv -> MetaCtx -> Level -> Value -> Term
quote tenv mctx l t = case force tenv mctx t of
  VRigid x sp -> quoteSpine tenv mctx l (Var (levelToIndex l x)) sp
  VFlex m sp -> quoteSpine tenv mctx l (Meta m) sp
  VTop x sp -> quoteSpine tenv mctx l (Top x) sp
  VTopAmb x sp -> quoteSpine tenv mctx l (TopAmb x) sp
  VU -> U
  VPi x a b -> Pi x (quote tenv mctx l a) (quoteBind tenv mctx l b)
  VLam x t -> Lam x (quoteBind tenv mctx l t)
  VSigma x a b -> Sigma x (quote tenv mctx l a) (quoteBind tenv mctx l b)
  VPair t u -> Pair (quote tenv mctx l t) (quote tenv mctx l u)
  VBrave t sp -> quoteSpine tenv mctx l (quote tenv mctx l t) sp

quoteBind :: TopEnv -> MetaCtx -> Level -> (Value -> Value) -> Term
quoteBind tenv mctx l b = quote tenv mctx (l + 1) (b $ VVar l)

quoteSpine :: TopEnv -> MetaCtx -> Level -> Term -> Spine -> Term
quoteSpine tenv mctx l h = \case
  SNil -> h
  SApp sp u -> quoteSpine tenv mctx l h sp `App` quote tenv mctx l u
  SProj1 sp -> Proj1 $ quoteSpine tenv mctx l h sp
  SProj2 sp -> Proj2 $ quoteSpine tenv mctx l h sp

quoteNondet :: TopEnv -> MetaCtx -> Level -> Value -> [(Term, MetaCtx)]
quoteNondet tenv mctx l t = do
  (t, mctx) <- forceNondet tenv mctx t
  case t of
    VRigid x sp -> quoteSpineNondet tenv mctx l (Var (levelToIndex l x)) sp
    VFlex m sp -> quoteSpineNondet tenv mctx l (Meta m) sp
    VTop x sp -> quoteSpineNondet tenv mctx l (Top x) sp
    VTopAmb x sp -> quoteSpineNondet tenv mctx l (TopAmb x) sp
    VU -> pure (U, mctx)
    VPi x a b -> do
      (a, mctx) <- quoteNondet tenv mctx l a
      (b, mctx) <- quoteBindNondet tenv mctx l b
      pure (Pi x a b, mctx)
    VLam x t -> do
      (t, mctx) <- quoteBindNondet tenv mctx l t
      pure (Lam x t, mctx)
    VSigma x a b -> do
      (a, mctx) <- quoteNondet tenv mctx l a
      (b, mctx) <- quoteBindNondet tenv mctx l b
      pure (Sigma x a b, mctx)
    VPair t u -> do
      (t, mctx) <- quoteNondet tenv mctx l t
      (u, mctx) <- quoteNondet tenv mctx l u
      pure (Pair t u, mctx)
    VBrave {} -> []

quoteBindNondet :: TopEnv -> MetaCtx -> Level -> (Value -> Value) -> [(Term, MetaCtx)]
quoteBindNondet tenv mctx l b = quoteNondet tenv mctx (l + 1) (b $ VVar l)

quoteSpineNondet :: TopEnv -> MetaCtx -> Level -> Term -> Spine -> [(Term, MetaCtx)]
quoteSpineNondet tenv mctx l h = \case
  SNil -> pure (h, mctx)
  SApp sp u -> do
    (t, mctx) <- quoteSpineNondet tenv mctx l h sp
    (u, mctx) <- quoteNondet tenv mctx l u
    pure (App t u, mctx)
  SProj1 sp -> do
    (t, mctx) <- quoteSpineNondet tenv mctx l h sp
    pure (Proj1 t, mctx)
  SProj2 sp -> do
    (t, mctx) <- quoteSpineNondet tenv mctx l h sp
    pure (Proj2 t, mctx)

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
                Solved t _ -> Just t
                Unsolved _ -> Nothing
        ]
      ++ [ pretty x <+> case xs of
             S1.Singleton x' -> "=" <+> pretty x'
             _ -> "∈" <+> align (list (fmap pretty $ NE.toList $ S1.toList xs))
         | (x, xs) <- M.toList mctx.resol
         ]

instance Pretty ((TopEnv, MetaCtx, Level) ⊢ Value) where
  pretty ((tenv, mctx, lvl) :⊢ v) = pretty $ quote tenv mctx lvl v

instance Pretty ((TopEnv, MetaCtx, [Name]) ⊢ Value) where
  pretty ((tenv, mctx, ns) :⊢ v) = pretty (ns :⊢ quote tenv mctx (coerce $ length ns) v)
