module Aegle.Search.Matching
  ( match0,
    match,
  )
where

import Aegle.Core.Evaluation
import Aegle.Core.Name
import Aegle.Core.Term
import Aegle.Prelude
import Aegle.Search.Matching.Pruning
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Set qualified as S

--------------------------------------------------------------------------------
-- Solving flex/rigid

-- Flex/rigid

-- | @(Γ : Cxt) → (spine : Sub Γ Δ) → PRen Δ Γ@.
--   Optionally returns a pruning of nonlinear spine entries, if there's any.
invert :: MetaCtx -> Level -> Spine -> Maybe (PartialRenaming, Maybe Pruning)
invert mctx gamma sp = do
  let go = \case
        SNil -> pure (0, mempty, mempty, [])
        SApp sp (force mctx -> VVar (Level x)) -> do
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
  pren <- invert mctx gamma sp
  solveWithPren tenv mctx m pren rhs

--------------------------------------------------------------------------------

match0 :: TopEnv -> MetaCtx -> "pat" :! Term -> "term" :! Term -> [MetaCtx]
match0 tenv mctx (Arg p) (Arg t) = do
  let vp = eval tenv mctx [] p
      vt = eval tenv mctx [] t
  match tenv mctx 0 ! #pat vp ! #term vt

match :: TopEnv -> MetaCtx -> Level -> "pat" :! Value -> "term" :! Value -> [MetaCtx]
match tenv mctx l (Arg p) (Arg t) = case (force mctx p, force mctx t) of
  (_, VFlex {}) -> error "match: metavariable in term"
  (VBrave {}, _) -> []
  (_, VBrave {}) -> []
  (VPi _ pa pb, VPi _ a b) -> do
    mctx <- match tenv mctx l ! #pat pa ! #term a
    match tenv mctx (l + 1) ! #pat (pb $ VVar l) ! #term (b $ VVar l)
  (VU, VU) -> pure mctx
  (VLam _ pt, VLam _ t) ->
    match tenv mctx (l + 1) ! #pat (pt $ VVar l) ! #term (t $ VVar l)
  (p, VLam _ pt) ->
    match tenv mctx (l + 1) ! #pat (p $$ VVar l) ! #term (pt $ VVar l)
  (VLam _ pt, t) ->
    match tenv mctx (l + 1) ! #pat (pt $ VVar l) ! #term (t $$ VVar l)
  (VSigma _ pa pb, VSigma _ a b) -> do
    mctx <- match tenv mctx l ! #pat pa ! #term a
    match tenv mctx (l + 1) ! #pat (pb $ VVar l) ! #term (b $ VVar l)
  (VPair pt pu, VPair t u) -> do
    mctx <- match tenv mctx l ! #pat pt ! #term t
    match tenv mctx l ! #pat pu ! #term u
  (VPair pt pu, t) -> do
    mctx <- match tenv mctx l ! #pat pt ! #term (vProj1 t)
    match tenv mctx l ! #pat pu ! #term (vProj2 t)
  (pt, VPair t u) -> do
    mctx <- match tenv mctx l ! #pat (vProj1 pt) ! #term t
    match tenv mctx l ! #pat (vProj2 pt) ! #term u
  (VRigid px psp, VRigid x sp)
    | px == x -> matchSpine tenv mctx l ! #pat psp ! #term sp
  (VOpaque px psp, VOpaque x sp)
    | px == x -> matchSpine tenv mctx l ! #pat psp ! #term sp
  (VFlex m psp, t) -> maybeToList $ solve tenv mctx l m psp t
  (VAmb px psp pxs [], VOpaque x sp)
    | x `S.member` pxs -> do
        let mctx' = resolveOpaque mctx px x
        matchSpine tenv mctx' l ! #pat psp ! #term sp
  (VOpaque px psp, VAmb x sp xs [])
    | px `S.member` xs -> do
        let mctx' = resolveOpaque mctx x px
        matchSpine tenv mctx' l ! #pat psp ! #term sp
  -- we don't take intersection currently
  (VAmb px psp _ [], VAmb x sp _ [])
    | px == x -> matchSpine tenv mctx l ! #pat psp ! #term sp
  (VAmb px psp pxs pts@(_ : _), t) -> do
    (p, mctx) <- chooseAmb mctx px psp pxs pts
    match tenv mctx l ! #pat p ! #term t
  (p, VAmb x sp xs ts@(_ : _)) -> do
    (t, mctx) <- chooseAmb mctx x sp xs ts
    match tenv mctx l ! #pat p ! #term t
  _ -> []

matchSpine :: TopEnv -> MetaCtx -> Level -> "pat" :! Spine -> "term" :! Spine -> [MetaCtx]
matchSpine tenv mctx l (Arg psp) (Arg sp) = case (psp, sp) of
  (SNil, SNil) -> pure mctx
  (SApp psp p, SApp sp t) -> do
    mctx <- matchSpine tenv mctx l ! #pat psp ! #term sp
    match tenv mctx l ! #pat p ! #term t
  (SProj1 psp, SProj1 sp) -> matchSpine tenv mctx l ! #pat psp ! #term sp
  (SProj2 psp, SProj2 sp) -> matchSpine tenv mctx l ! #pat psp ! #term sp
  _ -> []
