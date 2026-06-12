module Aegle.Search.Unification
  ( unify0,
    unify,
  )
where

import Aegle.Core.Evaluation
import Aegle.Core.Name
import Aegle.Core.Term hiding (rename)
import Aegle.Prelude
import Aegle.Search.Unification.Pruning
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Prettyprinter

--------------------------------------------------------------------------------
-- Flex/rigid

-- | @(Γ : Cxt) → (spine : Sub Γ Δ) → PRen Δ Γ@.
--   Optionally returns a pruning of nonlinear spine entries, if there's any.
invert :: TopEnv -> MetaCtx -> Level -> Spine -> Maybe (PartialRenaming, Maybe Pruning)
invert tenv mctx gamma sp = do
  let go = \case
        SNil -> pure (0, mempty, mempty, [])
        SApp sp (force tenv mctx -> VVar (Level x)) -> do
          (dom, ren, nlvars, fsp) <- go sp
          case IM.member x ren || IS.member x nlvars of
            True -> pure (dom + 1, IM.delete x ren, IS.insert x nlvars, Level x : fsp)
            False -> pure (dom + 1, IM.insert x dom ren, nlvars, Level x : fsp)
        SApp {} -> Nothing
        SProj1 {} -> Nothing
        SProj2 {} -> Nothing

  (dom, ren, nlvars, fsp) <- go sp

  let mask = map \(Level x) -> IS.notMember x nlvars

  pure (PRen Nothing dom gamma ren, mask fsp <$ guard (not $ IS.null nlvars))

-- | Solve @Γ ⊢ m spine =? rhs@.
solve :: TopEnv -> MetaCtx -> Level -> MetaVar -> Spine -> Value -> Maybe MetaCtx
solve tenv mctx gamma m sp rhs = do
  pren <- invert tenv mctx gamma sp
  solveWithPren tenv mctx m pren rhs

--------------------------------------------------------------------------------

unify0 :: TopEnv -> MetaCtx -> Term -> Term -> [MetaCtx]
unify0 tenv mctx t t' = do
  let v = eval tenv mctx [] t
      v' = eval tenv mctx [] t'
  unify tenv mctx 0 v v'

unify :: TopEnv -> MetaCtx -> Level -> Value -> Value -> [MetaCtx]
unify tenv mctx l t t' | traceUnify tenv mctx l t t' = undefined
unify tenv mctx l t t' = case (force tenv mctx t, force tenv mctx t') of
  (VBrave {}, _) -> []
  (_, VBrave {}) -> []
  (VPi _ a b, VPi _ a' b') -> do
    mctx <- unify tenv mctx l a a'
    unify tenv mctx (l + 1) (b $ VVar l) (b' $ VVar l)
  (VU, VU) -> pure mctx
  (VLam _ t, VLam _ t') ->
    unify tenv mctx (l + 1) (t $ VVar l) (t' $ VVar l)
  (t, VLam _ t') ->
    unify tenv mctx (l + 1) (t $$ VVar l) (t' $ VVar l)
  (VLam _ t, t') ->
    unify tenv mctx (l + 1) (t $ VVar l) (t' $$ VVar l)
  (VSigma _ a b, VSigma _ a' b') -> do
    mctx <- unify tenv mctx l a a'
    unify tenv mctx (l + 1) (b $ VVar l) (b' $ VVar l)
  (VPair t u, VPair t' u') -> do
    mctx <- unify tenv mctx l t t'
    unify tenv mctx l u u'
  (VPair t u, t') -> do
    mctx <- unify tenv mctx l t (vProj1 t')
    unify tenv mctx l u (vProj2 t')
  (t, VPair t' u') -> do
    mctx <- unify tenv mctx l (vProj1 t) t'
    unify tenv mctx l (vProj2 t) u'
  (VRigid x sp, VRigid x' sp')
    | x == x' -> unifySpine tenv mctx l sp sp'
  (VTop x sp, VTop x' sp')
    | x == x' -> unifySpine tenv mctx l sp sp'
  (VTopAmb x sp, VTopAmb x' sp')
    | x == x' -> unifySpine tenv mctx l sp sp'
  (VFlex m sp, VFlex m' sp')
    | m == m' -> unifySpine tenv mctx l sp sp'
  -- Currently, we try to solve flex/rigid even the rigid one is TopAmb
  -- TODO: is this really okay?
  (VFlex m sp, t') -> maybeToList $ solve tenv mctx l m sp t'
  (t, VFlex m' sp') -> maybeToList $ solve tenv mctx l m' sp' t
  (VTopAmb x sp, t') ->
    asum
      [ do
          VTop x' sp' <- pure t'
          mctx <- maybeToList $ resolve mctx x x'
          unifySpine tenv mctx l sp sp',
        do
          (t, mctx) <- expandNondet tenv mctx x sp
          unify tenv mctx l t t'
      ]
  (t, VTopAmb x' sp') ->
    asum
      [ do
          VTop x sp <- pure t
          mctx <- maybeToList $ resolve mctx x' x
          unifySpine tenv mctx l sp sp',
        do
          (t', mctx) <- expandNondet tenv mctx x' sp'
          unify tenv mctx l t t'
      ]
  _ -> []

unifySpine :: TopEnv -> MetaCtx -> Level -> Spine -> Spine -> [MetaCtx]
unifySpine tenv mctx l = \cases
  SNil SNil -> pure mctx
  (SApp sp t) (SApp sp' t') -> do
    mctx <- unifySpine tenv mctx l sp sp'
    unify tenv mctx l t t'
  (SProj1 sp) (SProj1 sp') -> unifySpine tenv mctx l sp sp'
  (SProj2 sp) (SProj2 sp') -> unifySpine tenv mctx l sp sp'
  _ _ -> []

--------------------------------------------------------------------------------
-- Debug

traceUnify :: TopEnv -> MetaCtx -> Level -> Value -> Value -> Bool
traceUnify tenv mctx l v v' = traceFalse $ show do
  vsep
    [ "unify",
      indent 4
        $ vsep
          [ "tenv" <+> colon <+> align (pretty tenv),
            "mctx" <+> colon <+> align (pretty (tenv :⊢ mctx)),
            "ctx size" <+> colon <+> pretty l,
            "lhs" <+> colon <+> pretty ((tenv, mctx, l) :⊢ v),
            "rhs" <+> colon <+> pretty ((tenv, mctx, l) :⊢ v')
          ]
    ]
