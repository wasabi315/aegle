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
import Data.Set.NonEmpty qualified as NES
import Data.Set.NonEmpty.Extra qualified as NES
import Prettyprinter

--------------------------------------------------------------------------------
-- Values

-- | Values
data Value
  = VRigid Level Spine
  | VFlex MetaVar Spine
  | VOpaque {-# UNPACK #-} QName Spine
  | VTopAmb TopEnv PQName Spine
  | VU
  | VPi Name VType (Value -> VType)
  | VLam Name (Value -> Value)
  | VSigma Name VType (Value -> VType)
  | VPair Value Value
  | VBrave Value Spine

{-# DEPRECATED VTopAmb "TODO: Delete" #-}

{-# DEPRECATED VBrave "TODO: Delete" #-}

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
type Resol = M.Map PQName (NES.NESet QName)

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

lookupResol :: MetaCtx -> PQName -> NES.NESet QName
lookupResol mctx n = mctx.resol M.! n

-- | Resolve an unresolved name @n@ as a canonical name @n'@.
-- Returns 'Nothing' if @n@ does not denote @n'@ according to the given 'MetaCtx'.
resolve :: MetaCtx -> PQName -> QName -> Maybe MetaCtx
resolve mctx n n' = do
  guard $ n' `NES.member` (mctx.resol M.! n)
  pure $! unsafeResolve mctx n n'

unsafeResolve :: MetaCtx -> PQName -> QName -> MetaCtx
unsafeResolve mctx n n' = unsafeRestrict mctx n (NES.singleton n')

unsafeRestrict :: MetaCtx -> PQName -> NES.NESet QName -> MetaCtx
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
vTop tenv n = ML.findWithDefault (VOpaque n SNil) n tenv

-- Reduce only when the name has been resolved
vTopAmb :: TopEnv -> MetaCtx -> PQName -> Value
vTopAmb tenv mctx n = case lookupResol mctx n of
  NES.Singleton n' -> vTop tenv n'
  _ -> VTopAmb tenv n SNil
{-# DEPRECATED vTopAmb "TODO: Delete" #-}

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
  VOpaque x sp -> VOpaque x (SApp sp u)
  VTopAmb tenv x sp -> VTopAmb tenv x (SApp sp u)
  VBrave b sp -> VBrave b (SApp sp u)
  t -> VBrave t (SApp SNil u)

vProj1 :: Value -> Value
vProj1 = \case
  VPair t _ -> t
  VRigid x sp -> VRigid x (SProj1 sp)
  VFlex m sp -> VFlex m (SProj1 sp)
  VOpaque x sp -> VOpaque x (SProj1 sp)
  VTopAmb tenv x sp -> VTopAmb tenv x (SProj1 sp)
  VBrave b sp -> VBrave b (SProj1 sp)
  t -> VBrave t (SProj1 SNil)

vProj2 :: Value -> Value
vProj2 = \case
  VPair _ t -> t
  VRigid x sp -> VRigid x (SProj2 sp)
  VFlex m sp -> VFlex m (SProj2 sp)
  VOpaque x sp -> VOpaque x (SProj2 sp)
  VTopAmb tenv x sp -> VTopAmb tenv x (SProj2 sp)
  VBrave b sp -> VBrave b (SProj2 sp)
  t -> VBrave t (SProj2 SNil)

instance HasField "p1" Value Value where
  getField = vProj1

instance HasField "p2" Value Value where
  getField = vProj2

vAppSpine :: Value -> Spine -> Value
vAppSpine t = \case
  SNil -> t
  SApp sp u -> vAppSpine t sp $$ u
  SProj1 sp -> vProj1 $ vAppSpine t sp
  SProj2 sp -> vProj2 $ vAppSpine t sp

force :: MetaCtx -> Value -> Value
force mctx = \case
  VFlex m sp
    | Solved t _ <- mctx.metaCtx IM.! coerce m ->
        force mctx (vAppSpine t sp)
  VTopAmb tenv n sp
    | NES.Singleton n' <- lookupResol mctx n ->
        force mctx (vAppSpine (vTop tenv n') sp)
  t -> t

expandNondet :: TopEnv -> MetaCtx -> PQName -> Spine -> [(Value, MetaCtx)]
expandNondet tenv mctx n sp = do
  n' <- toList $ lookupResol mctx n
  Just t <- pure $ ML.lookup n' tenv
  let mctx' = unsafeResolve mctx n n'
      v = vAppSpine t sp
  pure (v, mctx')

forceNondet :: MetaCtx -> Value -> [(Value, MetaCtx)]
forceNondet mctx = \case
  VFlex m sp
    | Solved t _ <- mctx.metaCtx IM.! coerce m ->
        forceNondet mctx (vAppSpine t sp)
  VTopAmb tenv n sp
    | NES.Singleton n' <- lookupResol mctx n ->
        forceNondet mctx (vAppSpine (vTop tenv n') sp)
  VTopAmb tenv n sp -> do
    asum
      [ do
          ns <- NES.withNonEmpty empty pure do
            NES.filter (`ML.notMember` tenv) (lookupResol mctx n)
          let mctx' = unsafeRestrict mctx n ns
          pure (VTopAmb tenv n sp, mctx'),
        expandNondet tenv mctx n sp
      ]
  t -> pure (t, mctx)

--------------------------------------------------------------------------------
-- Quotation

levelToIndex :: Level -> Level -> Index
levelToIndex (Level l) (Level x) = Index (l - x - 1)

quote :: MetaCtx -> Level -> Value -> Term
quote mctx l t = case force mctx t of
  VRigid x sp -> quoteSpine mctx l (Var (levelToIndex l x)) sp
  VFlex m sp -> quoteSpine mctx l (Meta m) sp
  VOpaque x sp -> quoteSpine mctx l (Top x) sp
  VTopAmb _ x sp -> quoteSpine mctx l (TopAmb x) sp
  VU -> U
  VPi x a b -> Pi x (quote mctx l a) (quoteBind mctx l b)
  VLam x t -> Lam x (quoteBind mctx l t)
  VSigma x a b -> Sigma x (quote mctx l a) (quoteBind mctx l b)
  VPair t u -> Pair (quote mctx l t) (quote mctx l u)
  VBrave t sp -> quoteSpine mctx l (quote mctx l t) sp

quoteBind :: MetaCtx -> Level -> (Value -> Value) -> Term
quoteBind mctx l b = quote mctx (l + 1) (b $ VVar l)

quoteSpine :: MetaCtx -> Level -> Term -> Spine -> Term
quoteSpine mctx l h = \case
  SNil -> h
  SApp sp u -> quoteSpine mctx l h sp `App` quote mctx l u
  SProj1 sp -> Proj1 $ quoteSpine mctx l h sp
  SProj2 sp -> Proj2 $ quoteSpine mctx l h sp

quoteNondet :: MetaCtx -> Level -> Value -> [(Term, MetaCtx)]
quoteNondet mctx l t = do
  (t, mctx) <- forceNondet mctx t
  case t of
    VRigid x sp -> quoteSpineNondet mctx l (Var (levelToIndex l x)) sp
    VFlex m sp -> quoteSpineNondet mctx l (Meta m) sp
    VOpaque x sp -> quoteSpineNondet mctx l (Top x) sp
    VTopAmb _ x sp -> quoteSpineNondet mctx l (TopAmb x) sp
    VU -> pure (U, mctx)
    VPi x a b -> do
      (a, mctx) <- quoteNondet mctx l a
      (b, mctx) <- quoteBindNondet mctx l b
      pure (Pi x a b, mctx)
    VLam x t -> do
      (t, mctx) <- quoteBindNondet mctx l t
      pure (Lam x t, mctx)
    VSigma x a b -> do
      (a, mctx) <- quoteNondet mctx l a
      (b, mctx) <- quoteBindNondet mctx l b
      pure (Sigma x a b, mctx)
    VPair t u -> do
      (t, mctx) <- quoteNondet mctx l t
      (u, mctx) <- quoteNondet mctx l u
      pure (Pair t u, mctx)
    VBrave {} -> []

quoteBindNondet :: MetaCtx -> Level -> (Value -> Value) -> [(Term, MetaCtx)]
quoteBindNondet mctx l b = quoteNondet mctx (l + 1) (b $ VVar l)

quoteSpineNondet :: MetaCtx -> Level -> Term -> Spine -> [(Term, MetaCtx)]
quoteSpineNondet mctx l h = \case
  SNil -> pure (h, mctx)
  SApp sp u -> do
    (t, mctx) <- quoteSpineNondet mctx l h sp
    (u, mctx) <- quoteNondet mctx l u
    pure (App t u, mctx)
  SProj1 sp -> do
    (t, mctx) <- quoteSpineNondet mctx l h sp
    pure (Proj1 t, mctx)
  SProj2 sp -> do
    (t, mctx) <- quoteSpineNondet mctx l h sp
    pure (Proj2 t, mctx)

--------------------------------------------------------------------------------
-- Prettyprinting

instance Pretty TopEnv where
  pretty tenv =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ [ pretty m
            <+> "="
            <+> pretty ((emptyMetaCtx mempty, Level 0) :⊢ t)
        | (m, t) <- ML.toList tenv
        ]

instance Pretty MetaCtx where
  pretty mctx =
    group
      $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
      $ [ pretty (MetaVar m)
            <+> "="
            <+> maybe "?" (pretty . ((mctx, Level 0) :⊢)) sol
        | (m, entry) <- IM.toList mctx.metaCtx,
          let sol = case entry of
                Solved t _ -> Just t
                Unsolved _ -> Nothing
        ]
      ++ [ pretty x <+> case xs of
             NES.Singleton x' -> "=" <+> pretty x'
             _ -> "∈" <+> align (list (fmap pretty $ NE.toList $ NES.toList xs))
         | (x, xs) <- M.toList mctx.resol
         ]

instance Pretty ((MetaCtx, Level) ⊢ Value) where
  pretty ((mctx, lvl) :⊢ v) = pretty $ quote mctx lvl v

instance Pretty ((MetaCtx, [Name]) ⊢ Value) where
  pretty ((mctx, ns) :⊢ v) = pretty (ns :⊢ quote mctx (coerce $ length ns) v)
