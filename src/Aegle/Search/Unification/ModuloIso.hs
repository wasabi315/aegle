module Aegle.Search.Unification.ModuloIso
  ( unifyIso0,
    unifyIso,
    assocSwap,
    currySwap,
  )
where

import Aegle.Core.Evaluation
import Aegle.Core.Isomorphism
import Aegle.Core.Name
import Aegle.Core.Term hiding (rename)
import Aegle.Prelude
import Aegle.Search.Unification
import Aegle.Search.Unification.Pruning
import Prettyprinter

--------------------------------------------------------------------------------
-- Rewriting types

-- | Pick up a domain without breaking dependencies.
pickUpDomain :: TopEnv -> MetaCtx -> Level -> Quant -> [(Quant, Iso, MetaCtx)]
pickUpDomain tenv mctx lvl (Quant x a b) = (Quant x a b, Refl, mctx) : go lvl b
  where
    idr = idPRen lvl
    ide = idEnv lvl

    go l c = case force tenv mctx $ c (VVar l) of
      VPi y c1 c2 ->
        asum
          [ do
              let i = l - lvl
              -- Strengthen c1. This may involve pruning.
              (c1, mctx) <- maybeToList $ rename tenv mctx (skipPRenN (i + 1) idr) c1
              let c1' = eval tenv mctx ide c1
                  rest ~vc1 = VPi x a (instPiAt i vc1 . b)
                  s = swaps i
              pure (Quant y c1' rest, s, mctx),
            go (l + 1) c2
          ]
      _ -> []

    instPiAt i ~v t = case (i, force tenv mctx t) of
      (0, VPi _ _ b) -> b v
      (i, VPi x a b) -> VPi x a (instPiAt (i - 1) v . b)
      _ -> impossible "pickUpDomain.instPiAt"

    swaps = \case
      0 -> PiSwap
      n -> piCongR (swaps (n - 1)) <> PiSwap

-- | Pick up a projection without breaking dependencies.
pickUpProjection :: TopEnv -> MetaCtx -> Level -> Quant -> [(Quant, Iso, MetaCtx)]
pickUpProjection tenv mctx lvl (Quant x a b) = (Quant x a b, Refl, mctx) : go lvl b
  where
    idr = idPRen lvl
    ide = idEnv lvl

    go l c = case force tenv mctx $ c (VVar l) of
      VSigma y c1 c2 ->
        asum
          [ do
              let i = l - lvl
              -- Strengthen c1. This may involve pruning.
              (c1, mctx) <- maybeToList $ rename tenv mctx (skipPRenN (i + 1) idr) c1
              let c1' = eval tenv mctx ide c1
                  rest ~vc1 = VSigma x a (instSigmaAt i vc1 . b)
                  s = swaps SigmaSwap i
              pure (Quant y c1' rest, s, mctx),
            go (l + 1) c2
          ]
      c -> do
        let i = l - l
        (c, mctx) <- maybeToList $ rename tenv mctx (skipPRenN (i + 1) idr) c
        let c' = eval tenv mctx ide c
            rest ~_ = dropLastProj (l + 1) (VSigma x a b)
            s = swaps Comm i
        pure (Quant "_" c' rest, s, mctx)

    instSigmaAt i ~v t = case (i, force tenv mctx t) of
      (0, VSigma _ _ b) -> b v
      (i, VSigma x a b) -> VSigma x a (instSigmaAt (i - 1) v . b)
      _ -> impossible "pickUpProjection.instSigmaAt"

    dropLastProj l t = case force tenv mctx t of
      VSigma x a b -> case b (VVar l) of
        VSigma {} -> VSigma x a (dropLastProj (l + 1) . b)
        _ -> a
      _ -> impossible "pickUpProjection.dropLastProj"

    swaps i = \case
      0 -> i
      n -> sigmaCongR (swaps i (n - 1)) <> SigmaSwap

-- | Pick a **non-sigma** projection without breaking dependencies.
-- This works even in the presence of arbitrarily nested sigmas in the type.
assocSwap :: TopEnv -> MetaCtx -> Level -> Quant -> [(Quant, Iso, MetaCtx)]
assocSwap tenv mctx lvl q = do
  -- Pick one projection first.
  (q, i, mctx) <- pickUpProjection tenv mctx lvl q
  case q of
    -- When the selected projection is a sigma type, we invoke
    -- assocSwap recursively to make the first projection of the sigma non-sigma!
    Quant x (VSigma y a b) c -> do
      (Quant y a b, j, mctx) <- assocSwap tenv mctx lvl (Quant y a b)
      let -- Then associate to make the first projection non-sigma.
          -- Note the transport along j!
          q = Quant y a \ ~u -> VSigma x (b u) \ ~v -> c (transportInv j (VPair u v))
          k = i <> sigmaCongL j <> Assoc
      pure (q, k, mctx)
    q -> pure (q, i, mctx)

-- | Pick a **non-sigma** domain without breaking dependencies.
-- This works even in the presence of arbitrarily nested sigmas in the type.

--   e.g) currySwap (List A → (B × A → A) × B → B) =
--          [ ( List A → (B × A → A) × B → B , Refl                    ),
--            ( (B × A → B) → B → List A → B , ΠSwap · Curry           ),
--            ( B → (B × A → B) → List A → B , ΠSwap · ΠL Comm · Curry )
--          ]
currySwap :: TopEnv -> MetaCtx -> Level -> Quant -> [(Quant, Iso, MetaCtx)]
currySwap tenv mctx lvl q = do
  (q, i, mctx) <- pickUpDomain tenv mctx lvl q
  case q of
    Quant x (VSigma y a b) c -> do
      (Quant y a b, j, mctx) <- assocSwap tenv mctx lvl (Quant y a b)
      let q = Quant y a \ ~u -> VPi x (b u) \ ~v -> c (transportInv j (VPair u v))
          k = i <> piCongL j <> Curry
      pure (q, k, mctx)
    q -> pure (q, i, mctx)

--------------------------------------------------------------------------------
-- Unification modulo type isomorphism

unifyIso0 :: TopEnv -> MetaCtx -> Term -> Term -> [(Iso, MetaCtx)]
unifyIso0 tenv mctx t t' = do
  let v = eval tenv mctx [] t
      v' = eval tenv mctx [] t'
  (i, i', mctx) <- unifyIso tenv mctx 0 v v'
  let j = i <> sym i'
  pure (j, mctx)

unifyIso :: TopEnv -> MetaCtx -> Level -> Value -> Value -> [(Iso, Iso, MetaCtx)]
unifyIso tenv mctx lvl t t' | traceUnifyIso tenv mctx lvl t t' = undefined
unifyIso tenv mctx lvl t t' = case (force tenv mctx t, force tenv mctx t') of
  (VBrave {}, _) -> []
  (_, VBrave {}) -> []
  (VPi x a b, VPi x' a' b') ->
    unifyPi tenv mctx lvl (Quant x a b) (Quant x' a' b')
  (VSigma x a b, VSigma x' a' b') ->
    unifySigma tenv mctx lvl (Quant x a b) (Quant x' a' b')
  (VTopAmb x sp, t') ->
    asum
      [ do
          (t, mctx) <- expandNondet tenv mctx x sp
          unifyIso tenv mctx lvl t t',
        do
          mctx <- unify tenv mctx lvl (VTopAmb x sp) t'
          pure (Refl, Refl, mctx)
      ]
  (t, VTopAmb x' sp') ->
    asum
      [ do
          (t', mctx) <- expandNondet tenv mctx x' sp'
          unifyIso tenv mctx lvl t t',
        do
          mctx <- unify tenv mctx lvl t (VTopAmb x' sp')
          pure (Refl, Refl, mctx)
      ]
  (t, t') -> do
    mctx <- unify tenv mctx lvl t t'
    pure (Refl, Refl, mctx)

unifyPi :: TopEnv -> MetaCtx -> Level -> Quant -> Quant -> [(Iso, Iso, MetaCtx)]
unifyPi tenv mctx lvl pi pi' = do
  let (Quant _ a b, i) = curry tenv mctx pi
  flip foldMapA (currySwap tenv mctx lvl pi') \(Quant _ a' b', i', mctx) -> do
    (ia, ia', mctx) <- unifyIso tenv mctx lvl a a'
    let v = transportInv ia (VVar lvl)
        v' = transportInv ia' (VVar lvl)
    (ib, ib', mctx) <- unifyIso tenv mctx (lvl + 1) (b v) (b' v')
    let j = i <> piCongL ia <> piCongR ib
        j' = i' <> piCongL ia' <> piCongR ib'
    pure (j, j', mctx)

unifySigma :: TopEnv -> MetaCtx -> Level -> Quant -> Quant -> [(Iso, Iso, MetaCtx)]
unifySigma tenv mctx lvl sig sig' = do
  let (Quant _ a b, i) = assoc tenv mctx sig
  flip foldMapA (assocSwap tenv mctx lvl sig') \(Quant _ a' b', i', mctx) -> do
    (ia, ia', mctx) <- unifyIso tenv mctx lvl a a'
    let v = transportInv ia (VVar lvl)
        v' = transportInv ia' (VVar lvl)
    (ib, ib', mctx) <- unifyIso tenv mctx (lvl + 1) (b v) (b' v')
    let j = i <> sigmaCongL ia <> sigmaCongR ib
        j' = i' <> sigmaCongL ia' <> sigmaCongR ib'
    pure (j, j', mctx)

--------------------------------------------------------------------------------

traceUnifyIso :: TopEnv -> MetaCtx -> Level -> Value -> Value -> Bool
traceUnifyIso tenv mctx l v v' = traceFalse $ show do
  vsep
    [ "unifyIso",
      indent 4
        $ vsep
          [ "tenv" <+> colon <+> align (pretty tenv),
            "mctx" <+> colon <+> align (pretty (tenv :⊢ mctx)),
            "ctx size" <+> colon <+> pretty l,
            "lhs" <+> colon <+> pretty ((tenv, mctx, l) :⊢ v),
            "rhs" <+> colon <+> pretty ((tenv, mctx, l) :⊢ v')
          ]
    ]
