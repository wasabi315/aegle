module Aegle.Search.DiscrimTree.Saturate
  ( saturate,
    saturate0,
    Resol,
    ResolEntry (..),
    SatLeaf (..),
  )
where

import Aegle.Core.Isomorphism hiding (transport, transportInv)
import Aegle.Core.Name
import Aegle.Prelude
import Aegle.Search.DiscrimTree
import Aegle.Search.Evaluation
import Aegle.Search.Query
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Prettyprinter

--------------------------------------------------------------------------------
-- Unfolding

-- | Name resolution decisions made
type Resol = M.Map PQName ResolEntry

data ResolEntry
  = Resolved QName
  | OpaqueOnly -- only when there are possible opaque names

-- | Resolve as far as existing resolution entries permit.
resolve :: Resol -> Value -> Value
resolve resol = \case
  t@(VResol x xs sp ts) -> case M.lookup x resol of
    Nothing -> t
    Just OpaqueOnly -> VResol x xs sp mempty
    Just (Resolved x') -> case M.lookup x' ts of
      Just t -> resolve resol t
      Nothing -> VOpaque x' sp
  t -> t

resolveNondet :: Resol -> Value -> [(Resol, Value)]
resolveNondet resol t = case resolve resol t of
  VResol x xs sp ts ->
    concat
      [ S.elems xs <&> \x' -> do
          let resol' = M.insert x (Resolved x') resol
          (resol', VOpaque x' sp),
        do
          (x', t) <- ML.assocs ts
          let resol' = M.insert x (Resolved x') resol
          resolveNondet resol' t
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

-- | (x0 : A0) ... (xn : An) -> B ~ (xn : An) (x0 : A0) ... -> B
piSwaps :: Int -> Iso
piSwaps = \case
  0 -> Refl
  n -> piCongR (piSwaps (n - 1)) <> PiSwap

-- | (x0 : A0) ... (xn : An) * B ~ (xn : An) (x0 : A0) ... * B
sigmaSwaps :: Int -> Iso
sigmaSwaps = \case
  0 -> Refl
  n -> sigmaCongR (sigmaSwaps (n - 1)) <> SigmaSwap

-- | (x0 : A0) ... (xn : An) * B ~ B * (x0 : A0) ... (xn : An)
sigmaSwapsLast :: Int -> Iso
sigmaSwapsLast = \case
  0 -> Comm
  n -> sigmaCongR (sigmaSwapsLast (n - 1)) <> SigmaSwap

instantiateNthPi :: Int -> Resol -> Value -> VType -> VType
instantiateNthPi i resol ~t = go i
  where
    go i u = case (i, resolve resol u) of
      (0, VPi _ _ b) -> b t
      (i, VPi x a b) -> VPi x a $ go (i - 1) . b
      _ -> impossible "instantiateNthPi"
{-# INLINE instantiateNthPi #-}

instantiateNthSigma :: Int -> Resol -> Value -> VType -> VType
instantiateNthSigma i resol ~t = go i
  where
    go i u = case (i, resolve resol u) of
      (0, VSigma _ _ b) -> b t
      (i, VSigma x a b) -> VSigma x a $ go (i - 1) . b
      _ -> impossible "instantiateNthSigma"
{-# INLINE instantiateNthSigma #-}

-- | Drop the final projection after the given number of nested sigmas.
dropLastProjection :: Int -> Resol -> VType -> VType
dropLastProjection i resol = go i
  where
    go i t = case (i, resolve resol t) of
      (0, VSigma _ a _) -> a
      (i, VSigma x a b)
        | i > 0 -> VSigma x a $ go (i - 1) . b
      _ -> impossible "dropLastProjection"
{-# INLINE dropLastProjection #-}

data Strengthen
  = OK
  | NeedResol [Resol]

strengthen :: Resol -> Level -> Level -> Value -> [Resol]
strengthen resol from to = unwrap resol . go to resol
  where
    go l resol t = case resolve resol t of
      VRigid x sp
        | from <= x && x < to -> NeedResol []
        | otherwise -> goSpine l resol sp
      VOpaque _ sp -> goSpine l resol sp
      VResol x _ sp ts -> case goSpine l resol sp of
        OK -> OK
        NeedResol resols ->
          NeedResol $ resols ++ do
            (x', t) <- ML.assocs ts
            let resol' = M.insert x (Resolved x') resol
            unwrap resol' (go l resol' t)
      VU -> OK
      VPi _ a b ->
        bind resol (go l resol a) \resol ->
          go (l + 1) resol (b $ VVar l)
      VLam _ t ->
        go (l + 1) resol (t $ VVar l)
      VSigma _ a b ->
        bind resol (go l resol a) \resol ->
          go (l + 1) resol (b $ VVar l)
      VPair t u ->
        bind resol (go l resol t) \resol ->
          go l resol u
      VBrave {} -> NeedResol []

    goSpine l resol = \case
      SNil -> OK
      SApp sp u ->
        bind resol (goSpine l resol sp) \resol ->
          go l resol u
      SProj1 sp -> goSpine l resol sp
      SProj2 sp -> goSpine l resol sp

    unwrap resol = \case
      OK -> [resol]
      NeedResol resols -> resols

    bind resol s k = case s of
      OK -> k resol
      NeedResol resols -> NeedResol do
        resol <- resols
        unwrap resol (k resol)
{-# INLINE strengthen #-}

-- | Pick up a domain without breaking dependencies.
pickUpDomain :: Resol -> Level -> Quant -> [(Resol, Quant, Iso)]
pickUpDomain resol l (Quant x a b) =
  (resol, Quant x a b, Refl) : go resol (l + 1) (b $ VVar l)
  where
    go resol l' t = case resolve resol t of
      VPi y c d ->
        concat
          [ do
              resol <- strengthen resol l l' c
              let rest ~t = VPi x a $ instantiateNthPi (coerce $ l' - l - 1) resol t . b
                  s = piSwaps $ coerce (l' - l)
              [(resol, Quant y c rest, s)],
            go resol (l' + 1) (d $ VVar l')
          ]
      VResol x _ _ ts -> do
        (x', t) <- ML.assocs ts
        let resol' = M.insert x (Resolved x') resol
        go resol' l' t
      VRigid {}; VOpaque {}; VU; VLam {}; VSigma {}; VPair {}; VBrave {} -> []
{-# INLINE pickUpDomain #-}

pickUpProjection :: Resol -> Level -> Quant -> [(Resol, Quant, Iso)]
pickUpProjection resol l (Quant x a b) =
  (resol, Quant x a b, Refl) : go resol (l + 1) (b $ VVar l)
  where
    go resol l' t = case resolve resol t of
      VSigma y c d ->
        concat
          [ do
              resol <- strengthen resol l l' c
              let rest ~u = VSigma x a $ instantiateNthSigma (coerce $ l' - l - 1) resol u . b
                  s = sigmaSwaps $ coerce (l' - l)
              pure (resol, Quant y c rest, s),
            go resol (l' + 1) (d $ VVar l')
          ]
      t@(VResol x xs _ ts) ->
        concat
          [ do
              guard $ not (S.null xs)
              let resol' = M.insert x OpaqueOnly resol
              goLast resol' l' t,
            do
              (x', t) <- ML.assocs ts
              let resol' = M.insert x (Resolved x') resol
              go resol' l' t
          ]
      VBrave {} -> []
      t -> goLast resol l' t

    goLast resol l' t = do
      resol <- strengthen resol l l' t
      let depth = coerce $ l' - l - 1
          rest ~_ = dropLastProjection depth resol (VSigma x a b)
          s = sigmaSwapsLast depth
      pure (resol, Quant "_" t rest, s)
{-# INLINE pickUpProjection #-}

assocSwap :: Resol -> Level -> Quant -> [(Resol, Quant, Iso)]
assocSwap resol l = go resol
  where
    go resol sig = do
      (resol, sig, i) <- pickUpProjection resol l sig
      case sig of
        Quant x (VSigma y a b) c -> do
          (resol, Quant y a b, j) <- go resol (Quant y a b)
          let sig' = Quant y a \ ~t -> VSigma x (b t) \ ~u -> c (transportInv j (VPair t u))
              k = i <> sigmaCongL j <> Assoc
          pure (resol, sig', k)
        _ -> pure (resol, sig, i)
{-# INLINE assocSwap #-}

currySwap :: Resol -> Level -> Quant -> [(Resol, Quant, Iso)]
currySwap resol l = go resol
  where
    go resol pi = do
      (resol, pi, i) <- pickUpDomain resol l pi
      case pi of
        Quant x (VSigma y a b) c -> do
          (resol, Quant y a b, j) <- assocSwap resol l (Quant y a b)
          let pi' = Quant y a \ ~t -> VPi x (b t) \ ~u -> c (transportInv j (VPair t u))
              k = i <> piCongL j <> Curry
          [(resol, pi', k)]
        _ -> [(resol, pi, i)]
{-# INLINE currySwap #-}

--------------------------------------------------------------------------------
-- Discrimination tree saturated by possible domain/projection permutation and unfolding
-- Branches are expanded on demand

data SatLeaf = SatLeaf
  { resol :: Resol,
    iso :: Iso
  }

type SatDT = DiscrimTree (NE.NonEmpty SatLeaf)

saturate0 :: TopEnv -> Term -> SatDT
saturate0 tenv t = saturate mempty 0 (eval tenv [] t)

saturate :: Resol -> Level -> Value -> SatDT
saturate resol l t =
  saturate' resol l t \resol iso -> Leaf (NE.singleton SatLeaf {..})

-- Implemented in "subtree-passing" style to avoid quadratic behavior

saturate' :: Resol -> Level -> Value -> (Resol -> Iso -> SatDT) -> SatDT
saturate' resol l t sub =
  flip foldMap' (resolveNondet resol t) \(resol, t) -> case t of
    VPi x a b -> saturatePi resol l (Quant x a b) sub
    VSigma x a b -> saturateSigma resol l (Quant x a b) sub
    _ -> saturateRefl' resol l t (`sub` Refl)

saturatePi :: Resol -> Level -> Quant -> (Resol -> Iso -> SatDT) -> SatDT
saturatePi resol l pi sub = one TPi do
  flip foldMap' (currySwap resol l pi) \(resol, Quant _ a b, i) ->
    saturate' resol l a \resol ia ->
      saturate' resol (l + 1) (b $ transportInv ia (VVar l)) \resol ib ->
        sub resol $! i <> piCongL ia <> piCongR ib

saturateSigma :: Resol -> Level -> Quant -> (Resol -> Iso -> SatDT) -> SatDT
saturateSigma resol l sig sub = one TSigma do
  flip foldMap' (assocSwap resol l sig) \(resol, Quant _ a b, i) ->
    saturate' resol l a \resol ia ->
      saturate' resol (l + 1) (b $ transportInv ia (VVar l)) \resol ib ->
        sub resol $! i <> sigmaCongL ia <> sigmaCongR ib

saturateRefl :: Resol -> Level -> Value -> (Resol -> SatDT) -> SatDT
saturateRefl resol l t sub =
  flip foldMap' (resolveNondet resol t) \(resol, t) ->
    saturateRefl' resol l t sub

saturateRefl' :: Resol -> Level -> Value -> (Resol -> SatDT) -> SatDT
saturateRefl' resol l t sub = case t of
  VResol {} -> impossible "reflDiscrimTree'"
  VRigid x sp -> saturateEta resol l (TRigid x) sp sub
  VOpaque x sp -> saturateEta resol l (TOpaque x) sp sub
  VU -> one TU (sub resol)
  VPi _ a b -> one TPi do
    saturateRefl resol l a \resol ->
      saturateRefl resol (l + 1) (b $ VVar l) sub
  VLam _ t -> one TLam do
    saturateRefl resol (l + 1) (t $ VVar l) sub
  VSigma _ a b -> one TSigma do
    saturateRefl resol l a \resol ->
      saturateRefl resol (l + 1) (b $ VVar l) sub
  VPair t u -> one TPair do
    saturateRefl resol l t \resol ->
      saturateRefl resol l u sub
  VBrave {} -> mempty

saturateEta :: Resol -> Level -> (Int -> Token) -> Spine -> (Resol -> SatDT) -> SatDT
saturateEta resol l hd sp sub =
  fold
    [ saturateSpine resol l hd sp sub,
      one TEtaLam do
        saturateEta resol (l + 1) hd (SApp sp (VVar l)) sub,
      one TEtaPair do
        saturateEta resol l hd (SProj1 sp) \resol ->
          saturateEta resol l hd (SProj2 sp) sub
    ]

saturateSpine :: Resol -> Level -> (Int -> Token) -> Spine -> (Resol -> SatDT) -> SatDT
saturateSpine resol l hd sp sub = go resol 0 sp sub
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

--------------------------------------------------------------------------------
-- Prettyprint

instance Pretty ResolEntry where
  pretty = \case
    Resolved x -> pretty x
    OpaqueOnly -> "<opaque only>"

instance Pretty SatLeaf where
  pretty SatLeaf {..} =
    tupled
      [ encloseSep lbrace rbrace comma do
          [pretty x <+> "↦" <+> pretty y | (x, y) <- M.assocs resol],
        pretty iso
      ]
