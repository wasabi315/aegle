module Aegle.Search.DiscriminationTree where

import Aegle.Core.Evaluation
import Aegle.Core.Isomorphism
import Aegle.Core.Name
import Aegle.Core.Term
import Aegle.Prelude
import Aegle.Search.Unification.Pruning (PartialRenaming (..), liftPRen)
import Data.IntMap.Strict qualified as IM
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set.NonEmpty qualified as S1
import Data.Text qualified as T

--------------------------------------------------------------------------------

data Token
  = TRigid Level Int -- spine length
  | TOpaque {-# UNPACK #-} QName Int -- spine length
  | TU
  | TPi
  | TLam
  | TSigma
  | TPair
  | TApp
  | TProj1
  | TProj2
  | TEtaLam
  | TEtaPair
  deriving stock (Eq, Ord, Show, Generic)

-- | Discrimination tree
data DT a
  = Leaf a
  | Empty
  | One Token ~(DT a)
  | Node (ML.Map Token (DT a)) -- two or more
  deriving stock (Functor, Foldable, Traversable)

--------------------------------------------------------------------------------
-- Discrimination tree "saturated" by permutation iso and possible unfolding

reflDT :: Resol -> Level -> Value -> (Resol -> DT a) -> DT a
reflDT resol l t k = case t of
  VRigid x sp -> etaDT resol l (TRigid x) sp k
  VOpaque x sp -> etaDT resol l (TOpaque x) sp k
  VTopAmb tenv x sp -> unions do
    x' <- S1.toList $ resol M.! x
    let resol' = M.insert x (S1.singleton x') resol
    pure case ML.lookup x' tenv of
      Nothing -> reflDT resol' l (VOpaque x' sp) k
      Just t -> reflDT resol' l (vAppSpine t sp) k
  VFlex {} -> impossible "reflTrie"
  VU -> One TU (k resol)
  VPi _ a b ->
    One TPi do
      reflDT resol l a \resol ->
        reflDT resol (l + 1) (b $ VVar l) k
  VLam _ t ->
    One TLam do
      reflDT resol (l + 1) (t $ VVar l) k
  VSigma _ a b ->
    One TSigma do
      reflDT resol l a \resol ->
        reflDT resol (l + 1) (b $ VVar l) k
  VPair t u ->
    One TPair do
      reflDT resol l t \resol ->
        reflDT resol l u k
  VBrave {} -> Empty

etaDT :: Resol -> Level -> (Int -> Token) -> Spine -> (Resol -> DT a) -> DT a
etaDT resol l hd sp k =
  reflDTSpine resol l hd sp k
    `union` One TEtaLam (etaDT resol (l + 1) hd (SApp sp (VVar l)) k)
    `union` One TEtaPair (etaDT resol l hd (SProj1 sp) \resol -> etaDT resol l hd (SProj2 sp) k)

reflDTSpine :: Resol -> Level -> (Int -> Token) -> Spine -> (Resol -> DT a) -> DT a
reflDTSpine resol l hd sp k = go resol 0 sp k
  where
    go resol len sp k = case sp of
      SNil -> One (hd len) (k resol)
      SApp sp u ->
        go resol (len + 1) sp \resol ->
          One TApp do
            reflDT resol l u k
      SProj1 sp ->
        go resol (len + 1) sp \resol ->
          One TProj1 (k resol)
      SProj2 sp ->
        go resol (len + 1) sp \resol ->
          One TProj2 (k resol)

unionWith :: (a -> a -> a) -> DT a -> DT a -> DT a
unionWith f = \cases
  Empty dt' -> dt'
  dt Empty -> dt
  (Leaf x) (Leaf y) -> Leaf $ f x y
  (One tok dt) (One tok' dt')
    | tok == tok' -> One tok $ unionWith f dt dt'
    | otherwise -> Node $ ML.fromList [(tok, dt), (tok', dt')]
  (One tok dt) (Node dts') -> Node $ ML.insertWith (unionWith f) tok dt dts'
  (Node dts) (One tok dt') -> Node $ ML.insertWith (flip $ unionWith f) tok dt' dts
  (Node dts) (Node dts') -> Node $ ML.unionWith (unionWith f) dts dts'
  _ _ -> error "impossible"

union :: DT a -> DT a -> DT a
union = unionWith const

unions :: (Foldable1 f) => f (DT a) -> DT a
unions = foldr1 union

--------------------------------------------------------------------------------
-- Matching

children :: DT a -> [(Token, DT a)]
children = \case
  Leaf {} -> impossible "children"
  Empty -> []
  One tok dt -> [(tok, dt)]
  Node dts -> ML.toAscList dts

child :: Token -> DT a -> Maybe (DT a)
child tok = \case
  Leaf {} -> impossible "child"
  Empty -> Nothing
  One tok' dt -> dt <$ guard (tok == tok')
  Node dts -> ML.lookup tok dts
{-# INLINE child #-}

spineLength :: Spine -> Int
spineLength = \case
  SNil -> 0
  SApp sp _ -> 1 + spineLength sp
  SProj1 sp -> 1 + spineLength sp
  SProj2 sp -> 1 + spineLength sp

match :: TopEnv -> MetaCtx -> Level -> Value -> DT Iso -> Maybe (MetaCtx, Iso)
match tenv mctx l t dt = match' tenv mctx l t dt \cases
  mctx (Leaf x) -> Just (mctx, x)
  _ Empty -> Nothing
  _ (One {}; Node {}) -> impossible "match"

match' ::
  TopEnv ->
  MetaCtx ->
  Level ->
  Value ->
  DT Iso ->
  (MetaCtx -> DT Iso -> Maybe (MetaCtx, Iso)) ->
  Maybe (MetaCtx, Iso)
match' tenv mctx l t dt k = case force mctx t of
  VRigid x sp ->
    asum
      [ do
          let len = spineLength sp
          dt <- child (TRigid x len) dt
          matchSpine tenv mctx l sp dt k,
        -- eta-expand
        -- don't descend TEtaLam and TEtaPair!
        do
          dt <- child TLam dt
          match' tenv mctx (l + 1) (t $$ VVar l) dt k,
        do
          dt <- child TPair dt
          match' tenv mctx l (vProj1 t) dt \mctx dt ->
            match' tenv mctx l (vProj2 t) dt k
      ]
  VOpaque x sp ->
    asum
      [ do
          let len = spineLength sp
          dt <- child (TOpaque x len) dt
          matchSpine tenv mctx l sp dt k,
        -- eta-expand
        -- don't descend TEtaLam and TEtaPair!
        do
          dt <- child TLam dt
          match' tenv mctx (l + 1) (t $$ VVar l) dt k,
        do
          dt <- child TPair dt
          match' tenv mctx l (vProj1 t) dt \mctx dt ->
            match' tenv mctx l (vProj2 t) dt k
      ]
  VFlex m sp -> do
    pren <- invert mctx l sp
    rename pren dt \rhs dt -> do
      let sol = eval tenv mctx [] $ lams pren.dom rhs
          mctx' = writeMeta mctx m sol (lookupUnsolved mctx m)
      k mctx' dt
  VTopAmb {} -> impossible "match'"
  VU -> do
    dt <- child TU dt
    k mctx dt
  -- TODO: m may solve to a sigma which unblocks currying
  -- VPi x (force mctx -> VFlex m sp) b -> error "TODO"
  VPi _ a b -> do
    dt <- child TPi dt
    match' tenv mctx l a dt \mctx dt ->
      match' tenv mctx (l + 1) (b $ VVar l) dt k
  VLam _ t ->
    asum
      [ do
          dt <- child TLam dt
          match' tenv mctx (l + 1) (t $ VVar l) dt k,
        -- eta-expand
        do
          dt <- child TEtaLam dt
          match' tenv mctx (l + 1) (t $ VVar l) dt k
      ]
  -- m may solve to a sigma which unblocks assoc
  -- VSigma x (force mctx -> VFlex m sp) b -> error "TODO"
  VSigma _ a b -> do
    dt <- child TSigma dt
    match' tenv mctx l a dt \mctx dt ->
      match' tenv mctx (l + 1) (b $ VVar l) dt k
  VPair t u ->
    asum
      [ do
          dt <- child TPair dt
          match' tenv mctx l t dt \mctx dt ->
            match' tenv mctx l u dt k,
        -- eta-expand
        do
          dt <- child TEtaPair dt
          match' tenv mctx l t dt \mctx dt ->
            match' tenv mctx l u dt k
      ]
  VBrave {} -> Nothing

matchSpine :: TopEnv -> MetaCtx -> Level -> Spine -> DT Iso -> (MetaCtx -> DT Iso -> Maybe (MetaCtx, Iso)) -> Maybe (MetaCtx, Iso)
matchSpine tenv mctx l sp dt k = case sp of
  SNil -> k mctx dt
  SApp sp u ->
    matchSpine tenv mctx l sp dt \mctx dt -> do
      dt <- child TApp dt
      match' tenv mctx l u dt k
  SProj1 sp ->
    matchSpine tenv mctx l sp dt \mctx dt -> do
      dt <- child TProj1 dt
      k mctx dt
  SProj2 sp ->
    matchSpine tenv mctx l sp dt \mctx dt -> do
      dt <- child TProj2 dt
      k mctx dt

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

rename ::
  PartialRenaming ->
  DT Iso ->
  (Term -> DT Iso -> Maybe a) ->
  Maybe a
rename pren dt k = asum do
  (tok, dt) <- children dt
  pure case tok of
    TRigid (Level x) len -> do
      x' <- IM.lookup x pren.ren
      renameSpine pren (Var $ levelToIndex pren.dom x') len dt k
    TOpaque x len -> renameSpine pren (Top x) len dt k
    TU -> k U dt
    TPi ->
      rename pren dt \a dt ->
        rename (liftPRen pren) dt \b dt ->
          k (Pi "x" a b) dt
    TLam ->
      rename (liftPRen pren) dt \t dt ->
        k (Lam "x" t) dt
    TSigma ->
      rename pren dt \a dt ->
        rename (liftPRen pren) dt \b dt ->
          k (Sigma "x" a b) dt
    TPair ->
      rename pren dt \t dt ->
        rename pren dt \u dt ->
          k (Pair t u) dt
    TApp; TProj1; TProj2 -> impossible "rename"
    TEtaLam; TEtaPair -> Nothing

renameSpine ::
  PartialRenaming ->
  Term ->
  Int ->
  DT Iso ->
  (Term -> DT Iso -> Maybe a) ->
  Maybe a
renameSpine _ hd 0 dt k = k hd dt
renameSpine pren hd len dt k = asum do
  (tok, dt) <- children dt
  pure case tok of
    TApp ->
      rename pren dt \u dt ->
        renameSpine pren (App hd u) (len - 1) dt k
    TProj1 ->
      renameSpine pren (Proj1 hd) (len - 1) dt k
    TProj2 ->
      renameSpine pren (Proj2 hd) (len - 1) dt k
    TRigid {}; TOpaque {}; TU; TPi; TLam; TSigma; TPair; TEtaLam; TEtaPair -> impossible "renameSpine"

lams :: Level -> Term -> Term
lams l = go 0
  where
    go x t | x == l = t
    go x t = Lam (Name $ "x" <> T.show (x + 1)) $ go (x + 1) t
