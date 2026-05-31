module TypeSearch.Core.Evaluation where

import Data.IntMap.Strict qualified as IM
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set.NonEmpty qualified as S1
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Prelude

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
    resolCtx :: M.Map PQName (S1.NESet QName)
  }

data MetaEntry
  = Unsolved ~Value
  | Solved Value ~Value

emptyMetaCtx :: M.Map PQName (S1.NESet QName) -> MetaCtx
emptyMetaCtx = MetaCtx 0 mempty

allMetaSolved :: MetaCtx -> Bool
allMetaSolved mctx = flip all mctx.metaCtx \case
  Unsolved {} -> False
  Solved {} -> True

--------------------------------------------------------------------------------
-- Evaluation

eval :: MetaCtx -> TopEnv -> Env -> Term -> Value
eval mctx tenv env = \case
  Var (Index x) -> env !! x
  Meta m -> vMeta mctx m
  Top x -> fromMaybe (VTop x SNil) $ tenv ML.!? x
  TopAmb x -> vTopAmb mctx x
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

-- TODO: canonicity? ambiguous names does not reduce
vTopAmb :: MetaCtx -> PQName -> Value
vTopAmb mctx n = case mctx.resolCtx M.! n of
  ns
    | S1.size ns == 1 -> VTop (S1.findMin ns) SNil
    | otherwise -> VTopAmb n SNil

vAppPruning :: Env -> Value -> Pruning -> Value
vAppPruning env ~v pr = case (env, pr) of
  ([], []) -> v
  (t : env, True : pr) -> vAppPruning env v pr $$ t
  (_ : env, False : pr) -> vAppPruning env v pr
  _ -> impossible

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

force :: MetaCtx -> Value -> Value
force mctx = \case
  VFlex m sp
    | Solved t _ <- mctx.metaCtx IM.! coerce m -> force mctx (vAppSpine t sp)
  VTopAmb n sp
    -- TODO: canonicity? ambiguous names does not reduce
    | ns <- mctx.resolCtx M.! n,
      S1.size ns == 1 ->
        VTop (S1.findMin ns) sp
  t -> t

--------------------------------------------------------------------------------
-- Quotation

levelToIndex :: Level -> Level -> Index
levelToIndex (Level l) (Level x) = Index (l - x - 1)

quote :: MetaCtx -> Level -> Value -> Term
quote mctx l t = case force mctx t of
  VRigid x sp -> quoteSpine mctx l (Var (levelToIndex l x)) sp
  VFlex m sp -> quoteSpine mctx l (Meta m) sp
  VTop x sp -> quoteSpine mctx l (Top x) sp
  VTopAmb x sp -> quoteSpine mctx l (TopAmb x) sp
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
