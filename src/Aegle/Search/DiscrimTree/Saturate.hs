module Aegle.Search.DiscrimTree.Saturate where

import Aegle.Core.Isomorphism hiding (transport, transportInv)
import Aegle.Core.Name
import Aegle.Prelude
import Aegle.Search.DiscrimTree
import Aegle.Search.Evaluation
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set qualified as S

--------------------------------------------------------------------------------
-- Unfolding

-- | Resolution decisions made
type Resol = M.Map PQName QName

resolve :: Resol -> Value -> [(Resol, Value)]
resolve resol = \case
  VResol x xs sp ts -> case M.lookup x resol of
    Just x'
      | x' `S.member` xs -> [(resol, VOpaque x' sp)]
      | otherwise -> resolve resol (ts ML.! x')
    Nothing ->
      concat
        [ do
            x' <- S.elems xs
            let resol' = M.insert x x' resol
            pure (resol', VOpaque x' sp),
          do
            (x', t) <- ML.assocs ts
            let resol' = M.insert x x' resol
            resolve resol' t
        ]
  t -> [(resol, t)]

--------------------------------------------------------------------------------
-- Transport

-- transport along an isomorphism
transport :: Iso -> Value -> Value
transport i v = case i of
  Refl -> v
  Sym i -> transportInv i v
  Trans i j -> transport j (transport i v)
  Assoc -> v.p1.p1 `VPair` (v.p1.p2 `VPair` v.p2)
  Comm -> v.p2 `VPair` v.p1
  SigmaSwap -> v.p2.p1 `VPair` (v.p1 `VPair` v.p2.p2)
  Curry -> VLam "x" \x -> VLam "y" \y -> v $$ VPair x y
  PiSwap -> VLam "y" \y -> VLam "x" \x -> v $$ x $$ y
  PiCongL i -> VLam "x" \x -> v $$ transportInv i x
  PiCongR i -> VLam "x" \x -> transport i (v $$ x)
  SigmaCongL i -> transport i v.p1 `VPair` v.p2
  SigmaCongR i -> v.p1 `VPair` transport i v.p2

-- transport back
transportInv :: Iso -> Value -> Value
transportInv i v = case i of
  Refl -> v
  Sym i -> transport i v
  Trans i j -> transportInv i (transportInv j v)
  Assoc -> (v.p1 `VPair` v.p2.p1) `VPair` v.p2.p2
  Comm -> v.p2 `VPair` v.p1
  SigmaSwap -> v.p2.p1 `VPair` (v.p1 `VPair` v.p2.p2)
  Curry -> VLam "p" \p -> v $$ p.p1 $$ p.p2
  PiSwap -> VLam "x" \x -> VLam "y" \y -> v $$ y $$ x
  PiCongL i -> VLam "x" \x -> v $$ transport i x
  PiCongR i -> VLam "x" \x -> transportInv i (v $$ x)
  SigmaCongL i -> transportInv i v.p1 `VPair` v.p2
  SigmaCongR i -> v.p1 `VPair` transportInv i v.p2

--------------------------------------------------------------------------------

pickUpDomain :: Resol -> Level -> Quant -> NE.NonEmpty (Quant, Iso, Resol)
pickUpDomain = undefined

pickUpProjection :: Resol -> Level -> Quant -> NE.NonEmpty (Quant, Iso, Resol)
pickUpProjection = undefined

assocSwap :: Resol -> Level -> Quant -> NE.NonEmpty (Quant, Iso, Resol)
assocSwap resol l = go resol
  where
    go resol sig = do
      (sig, i, resol) <- pickUpProjection resol l sig
      case sig of
        Quant x (VSigma y a b) c -> do
          (Quant y a b, j, resol) <- go resol (Quant y a b)
          let sig' = Quant y a \ ~t -> VSigma x (b t) \ ~u -> c (transportInv j (VPair t u))
              k = i <> sigmaCongL j <> Assoc
          pure (sig', k, resol)
        _ -> pure (sig, i, resol)

currySwap :: Resol -> Level -> Quant -> NE.NonEmpty (Quant, Iso, Resol)
currySwap resol l = go resol
  where
    go resol pi = do
      (pi, i, resol) <- pickUpDomain resol l pi
      case pi of
        Quant x (VSigma y a b) c -> do
          (Quant y a b, j, resol) <- go resol (Quant y a b)
          let pi' = Quant y a \ ~t -> VPi x (b t) \ ~u -> c (transportInv j (VPair t u))
              k = i <> piCongL j <> Curry
          pure (pi', k, resol)
        _ -> pure (pi, i, resol)

--------------------------------------------------------------------------------
-- Discrimination tree "saturated" by permutation iso and possible unfolding
-- Branches are expanded on demand

saturate :: Resol -> Level -> Value -> DiscrimTree (S.Set Iso)
saturate resol l t = saturate' resol l t \_ -> Leaf . S.singleton

saturate' ::
  Resol ->
  Level ->
  Value ->
  (Resol -> Iso -> DiscrimTree (S.Set Iso)) ->
  DiscrimTree (S.Set Iso)
saturate' resol l t k =
  flip foldMap' (resolve resol t) \(resol, t) -> case t of
    VPi x a b -> saturatePi resol l (Quant x a b) k
    VSigma x a b -> saturateSigma resol l (Quant x a b) k
    _ -> saturateRefl' resol l t (`k` Refl)

saturatePi ::
  Resol ->
  Level ->
  Quant ->
  (Resol -> Iso -> DiscrimTree (S.Set Iso)) ->
  DiscrimTree (S.Set Iso)
saturatePi resol l pi k = one TPi do
  flip foldMap' (currySwap resol l pi) \(Quant _ a b, i, resol) ->
    saturate' resol l a \resol ia ->
      saturate' resol (l + 1) (b $ transportInv ia (VVar l)) \resol ib ->
        k resol $! i <> piCongL ia <> piCongR ib

saturateSigma ::
  Resol ->
  Level ->
  Quant ->
  (Resol -> Iso -> DiscrimTree (S.Set Iso)) ->
  DiscrimTree (S.Set Iso)
saturateSigma resol l sig k = one TSigma do
  flip foldMap' (assocSwap resol l sig) \(Quant _ a b, i, resol) ->
    saturate' resol l a \resol ia ->
      saturate' resol (l + 1) (b $ transportInv ia (VVar l)) \resol ib ->
        k resol $! i <> sigmaCongL ia <> sigmaCongR ib

saturateRefl ::
  Resol ->
  Level ->
  Value ->
  (Resol -> DiscrimTree (S.Set Iso)) ->
  DiscrimTree (S.Set Iso)
saturateRefl resol l t k =
  flip foldMap' (resolve resol t) \(resol, t) -> saturateRefl' resol l t k

saturateRefl' ::
  Resol ->
  Level ->
  Value ->
  (Resol -> DiscrimTree (S.Set Iso)) ->
  DiscrimTree (S.Set Iso)
saturateRefl' resol l t k = case t of
  VResol {} -> impossible "reflDiscrimTree'"
  VRigid x sp -> saturateEta resol l (TRigid x) sp k
  VOpaque x sp -> saturateEta resol l (TOpaque x) sp k
  VU -> one TU (k resol)
  VPi _ a b -> one TPi do
    saturateRefl resol l a \resol ->
      saturateRefl resol (l + 1) (b $ VVar l) k
  VLam _ t -> one TLam do
    saturateRefl resol (l + 1) (t $ VVar l) k
  VSigma _ a b -> one TSigma do
    saturateRefl resol l a \resol ->
      saturateRefl resol (l + 1) (b $ VVar l) k
  VPair t u -> one TPair do
    saturateRefl resol l t \resol ->
      saturateRefl resol l u k
  VBrave {} -> mempty

saturateEta ::
  Resol ->
  Level ->
  (Int -> Token) ->
  Spine ->
  (Resol -> DiscrimTree (S.Set Iso)) ->
  DiscrimTree (S.Set Iso)
saturateEta resol l hd sp k =
  fold
    [ saturateSpine resol l hd sp k,
      one TEtaLam do
        saturateEta resol (l + 1) hd (SApp sp (VVar l)) k,
      one TEtaPair do
        saturateEta resol l hd (SProj1 sp) \resol ->
          saturateEta resol l hd (SProj2 sp) k
    ]

saturateSpine ::
  Resol ->
  Level ->
  (Int -> Token) ->
  Spine ->
  (Resol -> DiscrimTree (S.Set Iso)) ->
  DiscrimTree (S.Set Iso)
saturateSpine resol l hd sp k = go resol 0 sp k
  where
    go resol len sp k = case sp of
      SNil -> one (hd len) (k resol)
      SApp sp u ->
        go resol (len + 1) sp \resol ->
          one TApp do
            saturateRefl resol l u k
      SProj1 sp ->
        go resol (len + 1) sp (one TProj1 . k)
      SProj2 sp ->
        go resol (len + 1) sp (one TProj2 . k)
