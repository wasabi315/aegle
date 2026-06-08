module TypeSearch.Search.Unification.ModuloIso where

import TypeSearch.Core.Evaluation
import TypeSearch.Core.Isomorphism
import TypeSearch.Core.Name
import TypeSearch.Core.Term hiding (rename)
import TypeSearch.Prelude
import TypeSearch.Search.Unification

--------------------------------------------------------------------------------
-- Rewriting types

idEnv :: Level -> Env
idEnv l = VVar <$> (l - 1) `down` 0

-- | Pick up a domain without breaking dependencies.
pickUpDomain :: MetaCtx -> TopEnv -> Level -> Quant -> [(Quant, Iso, MetaCtx)]
pickUpDomain mctx tenv lvl (Quant x a b) = (Quant x a b, Refl, mctx) : go lvl b
  where
    idr = idPRen lvl
    ide = idEnv lvl

    go l c = case force mctx tenv $ c (VVar l) of
      VPi y c1 c2 ->
        ( do
            let i = l - lvl
            -- Strengthen c1. This may involve pruning.
            (c1, mctx) <- maybeToList $ rename mctx tenv (skipPrenN (i + 1) idr) c1
            let c1' = eval mctx tenv ide c1
                rest ~vc1 = VPi x a (instPiAt i vc1 . b)
                s = swaps i
            pure (Quant y c1' rest, s, mctx)
        )
          ++ go (l + 1) c2
      _ -> []

    instPiAt i ~v t = case (i, force mctx tenv t) of
      (0, VPi _ _ b) -> b v
      (i, VPi x a b) -> VPi x a (instPiAt (i - 1) v . b)
      _ -> impossible "pickUpDomain.instPiAt"

    swaps = \case
      0 -> PiSwap
      n -> piCongR (swaps (n - 1)) <> PiSwap

-- | Pick up a projection without breaking dependencies.
pickUpProjection :: MetaCtx -> TopEnv -> Level -> Quant -> [(Quant, Iso, MetaCtx)]
pickUpProjection mctx tenv lvl (Quant x a b) = (Quant x a b, Refl, mctx) : go lvl b
  where
    idr = idPRen lvl
    ide = idEnv lvl

    go l c = case force mctx tenv $ c (VVar l) of
      VSigma y c1 c2 ->
        ( do
            let i = l - lvl
            -- Strengthen c1. This may involve pruning.
            (c1, mctx) <- maybeToList $ rename mctx tenv (skipPrenN (i + 1) idr) c1
            let c1' = eval mctx tenv ide c1
                rest ~vc1 = VSigma x a (instSigmaAt i vc1 . b)
                s = swaps SigmaSwap i
            pure (Quant y c1' rest, s, mctx)
        )
          ++ go (l + 1) c2
      c -> do
        let i = l - l
        (c, mctx) <- maybeToList $ rename mctx tenv (skipPrenN (i + 1) idr) c
        let c' = eval mctx tenv ide c
            rest ~_ = dropLastProj (l + 1) (VSigma x a b)
            s = swaps Comm i
        pure (Quant "_" c' rest, s, mctx)

    instSigmaAt i ~v t = case (i, force mctx tenv t) of
      (0, VSigma _ _ b) -> b v
      (i, VSigma x a b) -> VSigma x a (instSigmaAt (i - 1) v . b)
      _ -> impossible "pickUpProjection.instSigmaAt"

    dropLastProj l t = case force mctx tenv t of
      VSigma x a b -> case b (VVar l) of
        VSigma {} -> VSigma x a (dropLastProj (l + 1) . b)
        _ -> a
      _ -> impossible "pickUpProjection.dropLastProj"

    swaps i = \case
      0 -> i
      n -> sigmaCongR (swaps i (n - 1)) <> SigmaSwap

-- | Pick a **non-sigma** projection without breaking dependencies.
-- This works even in the presence of arbitrarily nested sigmas in the type.
assocSwap :: MetaCtx -> TopEnv -> Level -> Quant -> [(Quant, Iso, MetaCtx)]
assocSwap mctx tenv lvl q = do
  -- Pick one projection first.
  (q, i, mctx) <- pickUpProjection mctx tenv lvl q
  case q of
    -- When the selected projection is a sigma type, we invoke
    -- assocSwap recursively to make the first projection of the sigma non-sigma!
    Quant x (VSigma y a b) c -> do
      (Quant y a b, j, mctx) <- assocSwap mctx tenv lvl (Quant y a b)
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
currySwap :: MetaCtx -> TopEnv -> Level -> Quant -> [(Quant, Iso, MetaCtx)]
currySwap mctx tenv lvl q = do
  (q, i, mctx) <- pickUpDomain mctx tenv lvl q
  case q of
    Quant x (VSigma y a b) c -> do
      (Quant y a b, j, mctx) <- assocSwap mctx tenv lvl (Quant y a b)
      let q = Quant y a \ ~u -> VPi x (b u) \ ~v -> c (transportInv j (VPair u v))
          k = i <> piCongL j <> Curry
      pure (q, k, mctx)
    q -> pure (q, i, mctx)

--------------------------------------------------------------------------------
-- Unification modulo type isomorphism

unifyIso0 :: MetaCtx -> TopEnv -> Term -> Term -> [(Iso, MetaCtx)]
unifyIso0 mctx tenv t t' = do
  let v = eval mctx tenv [] t
      v' = eval mctx tenv [] t'
  (i, i', mctx) <- unifyIso mctx tenv 0 v v'
  let j = i <> sym i'
  pure (j, mctx)

unifyIso :: MetaCtx -> TopEnv -> Level -> Value -> Value -> [(Iso, Iso, MetaCtx)]
unifyIso mctx tenv lvl t u = case (force mctx tenv t, force mctx tenv u) of
  (VBrave {}, _) -> []
  (_, VBrave {}) -> []
  (VPi x a b, VPi x' a' b') ->
    unifyPi mctx tenv lvl (Quant x a b) (Quant x' a' b')
  (VSigma x a b, VSigma x' a' b') ->
    unifySigma mctx tenv lvl (Quant x a b) (Quant x' a' b')
  (t, u) -> do
    mctx <- unify mctx tenv lvl t u
    pure (Refl, Refl, mctx)

unifyPi :: MetaCtx -> TopEnv -> Level -> Quant -> Quant -> [(Iso, Iso, MetaCtx)]
unifyPi mctx tenv lvl pi pi' = do
  let (Quant _ a b, i) = curry mctx tenv pi
  flip foldMapA (currySwap mctx tenv lvl pi') \(Quant _ a' b', i', mctx) -> do
    (ia, ia', mctx) <- unifyIso mctx tenv lvl a a'
    let v = transportInv ia (VVar lvl)
        v' = transportInv ia' (VVar lvl)
    (ib, ib', mctx) <- unifyIso mctx tenv (lvl + 1) (b v) (b' v')
    let j = i <> piCongL ia <> piCongR ib
        j' = i' <> piCongL ia' <> piCongR ib'
    pure (j, j', mctx)

unifySigma :: MetaCtx -> TopEnv -> Level -> Quant -> Quant -> [(Iso, Iso, MetaCtx)]
unifySigma mctx tenv lvl sig sig' = do
  let (Quant _ a b, i) = assoc mctx tenv sig
  flip foldMapA (assocSwap mctx tenv lvl sig') \(Quant _ a' b', i', mctx) -> do
    (ia, ia', mctx) <- unifyIso mctx tenv lvl a a'
    let v = transportInv ia (VVar lvl)
        v' = transportInv ia' (VVar lvl)
    (ib, ib', mctx) <- unifyIso mctx tenv (lvl + 1) (b v) (b' v')
    let j = i <> sigmaCongL ia <> sigmaCongR ib
        j' = i' <> sigmaCongL ia' <> sigmaCongR ib'
    pure (j, j', mctx)
