module Aegle.Search.Evaluation where

import Aegle.Core.Name
import Aegle.Core.Term qualified as C
import Aegle.Prelude
import Aegle.Search.Query
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set qualified as S

--------------------------------------------------------------------------------
-- Values

data Value
  = VRigid Level Spine
  | VOpaque {-# UNPACK #-} QName Spine
  | VResol PQName (S.Set QName) Spine (ML.Map QName Value) -- at least one is non-empty
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

data Quant = Quant Name Value (Value -> Value)

-- | Environment keyed by names
type Env = [(Name, Value)]

-- | Environment keyed by ambiguous names
type TopEnv = ML.Map PQName TopEnvEntry

data TopEnvEntry = TopEnvEntry
  { -- opaques and transps are disjoint and at least one is non-empty
    opaques :: S.Set QName,
    transps :: ML.Map QName Value
  }

--------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate core term.
-- Assumes no metavars and all top-level names are opaque.
evalCore :: [Value] -> C.Term -> Value
evalCore env = \case
  C.Var x -> env !! coerce x
  C.Top x -> VOpaque x SNil
  C.TopAmb {} -> error "to be deleted"
  C.U -> VU
  C.Pi x a b -> VPi x (evalCore env a) \ ~t -> evalCore (t : env) b
  C.Lam x t -> VLam x \u -> evalCore (u : env) t
  C.App t u -> evalCore env t $$ evalCore env u
  C.Sigma x a b -> VSigma x (evalCore env a) \ ~t -> evalCore (t : env) b
  C.Pair t u -> evalCore env t `VPair` evalCore env u
  C.Proj1 t -> (evalCore env t).p1
  C.Proj2 t -> (evalCore env t).p2
  C.Meta {}; C.AppPruning {} -> impossible "evalCore"

-- | Evaluate query term.
eval :: TopEnv -> Env -> Term -> Value
eval tenv env = \case
  Var (Unqual x) | Just t <- lookup x env -> t
  Var x -> do
    let TopEnvEntry {..} = tenv M.! x
    VResol x opaques SNil transps
  U -> VU
  Pi x a b -> VPi x (eval tenv env a) \ ~t -> eval tenv ((x, t) : env) b
  Lam x t -> VLam x \u -> eval tenv ((x, u) : env) t
  App t u -> eval tenv env t $$ eval tenv env u
  Sigma x a b -> VSigma x (eval tenv env a) \ ~t -> eval tenv ((x, t) : env) b
  Pair t u -> eval tenv env t `VPair` eval tenv env u
  Proj1 t -> (eval tenv env t).p1
  Proj2 t -> (eval tenv env t).p2

($$) :: Value -> Value -> Value
t $$ u = case t of
  VLam _ t -> t u
  VRigid x sp -> VRigid x (SApp sp u)
  VOpaque x sp -> VOpaque x (SApp sp u)
  VResol x xs sp ts -> VResol x xs (SApp sp u) (ts <&> ($$ u))
  VBrave t sp -> VBrave t (SApp sp u)
  _ -> VBrave t (SApp SNil u)

vproj1 :: Value -> Value
vproj1 = \case
  VPair t _ -> t
  VRigid x sp -> VRigid x (SProj1 sp)
  VOpaque x sp -> VOpaque x (SProj1 sp)
  VResol x xs sp ts -> VResol x xs (SProj1 sp) (vproj1 <$> ts)
  VBrave t sp -> VBrave t (SProj1 sp)
  t -> VBrave t (SProj1 SNil)

vproj2 :: Value -> Value
vproj2 = \case
  VPair _ t -> t
  VRigid x sp -> VRigid x (SProj2 sp)
  VOpaque x sp -> VOpaque x (SProj2 sp)
  VResol x xs sp ts -> VResol x xs (SProj2 sp) (vproj2 <$> ts)
  VBrave t sp -> VBrave t (SProj2 sp)
  t -> VBrave t (SProj2 SNil)

instance HasField "p1" Value Value where
  getField = vproj1
  {-# INLINE getField #-}

instance HasField "p2" Value Value where
  getField = vproj2
  {-# INLINE getField #-}
