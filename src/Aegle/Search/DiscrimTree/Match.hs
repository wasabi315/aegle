module Aegle.Search.DiscrimTree.Match where

import Aegle.Core.Evaluation
import Aegle.Core.Name
import Aegle.Core.Term
import Aegle.Prelude
import Aegle.Search.DiscrimTree
import Aegle.Search.Unification.Pruning (PartialRenaming (..))
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T

--------------------------------------------------------------------------------
-- Matching

spineLength :: Spine -> Int
spineLength = \case
  SNil -> 0
  SApp sp _ -> 1 + spineLength sp
  SProj1 sp -> 1 + spineLength sp
  SProj2 sp -> 1 + spineLength sp

match :: TopEnv -> MetaCtx -> Level -> Value -> DiscrimTree a -> [(MetaCtx, a)]
match tenv mctx l t dt = fmap extract <$> match' tenv mctx l t dt

match' :: TopEnv -> MetaCtx -> Level -> Value -> DiscrimTree a -> [(MetaCtx, DiscrimTree a)]
match' tenv mctx l t dt = case force mctx t of
  t@(VRigid x sp) ->
    asum
      [ do
          let len = spineLength sp
          dt <- maybeToList $ child (TRigid x len) dt
          matchSpine tenv mctx l sp dt,
        matchEta tenv mctx l t dt
      ]
  t@(VOpaque x sp) ->
    asum
      [ do
          let len = spineLength sp
          dt <- maybeToList $ child (TOpaque x len) dt
          matchSpine tenv mctx l sp dt,
        matchEta tenv mctx l t dt
      ]
  VFlex m sp -> solve tenv mctx l m sp dt
  VU -> do
    dt <- maybeToList $ child TU dt
    pure (mctx, dt)
  -- TODO: consider when a is metavar
  VPi _ a b -> do
    dt <- maybeToList $ child TPi dt
    (mctx, dt) <- match' tenv mctx l a dt
    match' tenv mctx (l + 1) (b $ VVar l) dt
  VLam _ t -> do
    dt <- mapMaybe (`child` dt) [TLam, TEtaLam]
    match' tenv mctx (l + 1) (t $ VVar l) dt
  -- TODO: consider when a is metavar
  VSigma _ a b -> do
    dt <- maybeToList $ child TSigma dt
    (mctx, dt) <- match' tenv mctx l a dt
    match' tenv mctx (l + 1) (b $ VVar l) dt
  VPair t u -> do
    dt <- mapMaybe (`child` dt) [TPair, TEtaPair]
    (mctx, dt) <- match' tenv mctx l t dt
    match' tenv mctx l u dt
  VTopAmb {}; VBrave {} -> error "to be deleted"

matchEta :: TopEnv -> MetaCtx -> Level -> Value -> DiscrimTree a -> [(MetaCtx, DiscrimTree a)]
matchEta tenv mctx l t dt =
  asum
    [ do
        dt <- maybeToList $ child TLam dt
        match' tenv mctx (l + 1) (t $$ VVar l) dt,
      do
        dt <- maybeToList $ child TPair dt
        (mctx, dt) <- match' tenv mctx l t.p1 dt
        match' tenv mctx l t.p2 dt
    ]

matchSpine :: TopEnv -> MetaCtx -> Level -> Spine -> DiscrimTree a -> [(MetaCtx, DiscrimTree a)]
matchSpine tenv mctx l sp dt = case sp of
  SNil -> pure (mctx, dt)
  SApp sp u -> do
    (mctx, dt) <- matchSpine tenv mctx l sp dt
    dt <- maybeToList $ child TApp dt
    match' tenv mctx l u dt
  SProj1 sp -> do
    (mctx, dt) <- matchSpine tenv mctx l sp dt
    dt <- maybeToList $ child TProj1 dt
    pure (mctx, dt)
  SProj2 sp -> do
    (mctx, dt) <- matchSpine tenv mctx l sp dt
    dt <- maybeToList $ child TProj2 dt
    pure (mctx, dt)

-- pattern unification
solve :: TopEnv -> MetaCtx -> Level -> MetaVar -> Spine -> DiscrimTree a -> [(MetaCtx, DiscrimTree a)]
solve tenv mctx l m sp dt = do
  pren <- maybeToList $ invert mctx l sp
  (rhs, dt) <- rename pren dt
  let sol = eval tenv mctx [] $ lams pren.dom rhs
  mctx <- pure $ writeMeta mctx m sol (lookupUnsolved mctx m)
  pure (mctx, dt)

invert :: MetaCtx -> Level -> Spine -> Maybe PartialRenaming
invert mctx l sp = do
  let go :: Spine -> Maybe (Level, IM.IntMap Level)
      go = \case
        SNil -> pure (0, mempty)
        SApp sp t -> do
          (dom, ren) <- go sp
          case force mctx t of
            VVar (Level x)
              | IM.notMember x ren ->
                  pure (dom + 1, IM.insert x dom ren)
            _ -> Nothing
        SProj1 {} -> Nothing
        SProj2 {} -> Nothing

  (dom, ren) <- go sp
  pure PRen {occ = Nothing, dom, cod = l, ren}

rename :: PartialRenaming -> DiscrimTree a -> [(Term, DiscrimTree a)]
rename pren dt = do
  (tok, dt) <- children dt
  case tok of
    TRigid (Level x) len -> do
      x' <- maybeToList $ IM.lookup x pren.ren
      renameSpine pren (Var $ levelToIndex pren.dom x') len dt
    TOpaque x len -> renameSpine pren (Top x) len dt
    TU -> pure (U, dt)
    TPi -> do
      (a, dt) <- rename pren dt
      (b, dt) <- rename pren.lift dt
      pure (Pi "x" a b, dt)
    TLam -> do
      (t, dt) <- rename pren.lift dt
      pure (Lam "x" t, dt)
    TSigma -> do
      (a, dt) <- rename pren dt
      (b, dt) <- rename pren.lift dt
      pure (Sigma "x" a b, dt)
    TPair -> do
      (t, dt) <- rename pren dt
      (u, dt) <- rename pren dt
      pure (Pair t u, dt)
    TApp; TProj1; TProj2 -> impossible "rename"
    TEtaLam; TEtaPair -> []

renameSpine :: PartialRenaming -> Term -> Int -> DiscrimTree a -> [(Term, DiscrimTree a)]
renameSpine _ hd 0 dt = pure (hd, dt)
renameSpine pren hd len dt = do
  (tok, dt) <- children dt
  case tok of
    TApp -> do
      (u, dt) <- rename pren dt
      renameSpine pren (App hd u) (len - 1) dt
    TProj1 ->
      renameSpine pren (Proj1 hd) (len - 1) dt
    TProj2 ->
      renameSpine pren (Proj2 hd) (len - 1) dt
    TRigid {}; TOpaque {}; TU; TPi; TLam; TSigma; TPair; TEtaLam; TEtaPair -> impossible "renameSpine"

lams :: Level -> Term -> Term
lams l = go 0
  where
    go x t | x == l = t
    go x t = Lam (Name $ "x" <> T.show (x + 1)) $ go (x + 1) t
