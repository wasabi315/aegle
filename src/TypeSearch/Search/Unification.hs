module TypeSearch.Search.Unification where

import Data.ImmatureStream qualified as IStr
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set.NonEmpty qualified as S1
import TypeSearch.Core.Evaluation
import TypeSearch.Core.Isomorphism
import TypeSearch.Core.Name
import TypeSearch.Core.Term hiding (rename)
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Unification context

data Ctx = Ctx
  { topEnv :: TopEnv,
    level :: Level,
    env :: Env,
    locals :: Locals,
    idRen :: PartialRenaming
  }

data Locals
  = Here
  | Bind Locals Name ~Term

-- | Partial renaming from @Γ@ to @Δ@.
data PartialRenaming = PRen
  { -- | optional occurs check.
    occ :: Maybe MetaVar,
    -- | size of @Γ@.
    dom :: Level,
    -- | size of @Δ@.
    cod :: Level,
    -- | mapping from @Δ@ vars to @Γ@ vars.
    ren :: IM.IntMap Level
  }

initCtx :: TopEnv -> Ctx
initCtx topEnv =
  Ctx
    { level = 0,
      env = [],
      locals = Here,
      idRen = emptyPRen,
      ..
    }

bind :: MetaCtx -> Ctx -> Name -> VType -> Ctx
bind mctx ctx@Ctx {..} x ~a =
  ctx
    { level = level + 1,
      env = VVar level : env,
      locals = Bind locals x (quote mctx topEnv level a),
      idRen = liftPren idRen
    }

--------------------------------------------------------------------------------
-- Metavar solving

emptyPRen :: PartialRenaming
emptyPRen = PRen Nothing 0 0 mempty

-- | @(σ : PRen Γ Δ) → PRen (Γ, x : A[σ]) (Δ, x : A)@.
liftPren :: PartialRenaming -> PartialRenaming
liftPren (PRen occ dom cod ren) =
  PRen occ (dom + 1) (cod + 1) (IM.insert (coerce cod) dom ren)

-- | @PRen Γ Δ → PRen Γ (Δ, x : A)@.
skipPren :: PartialRenaming -> PartialRenaming
skipPren (PRen occ dom cod ren) = PRen occ dom (cod + 1) ren

skipPrenN :: Level -> PartialRenaming -> PartialRenaming
skipPrenN n (PRen occ dom cod ren) = PRen occ dom (cod + n) ren

-- | @(Γ : Cxt) → (spine : Sub Γ Δ) → PRen Δ Γ@.
--   Optionally returns a pruning of nonlinear spine entries, if there's any.
invert :: MetaCtx -> TopEnv -> Level -> Spine -> Maybe (PartialRenaming, Maybe Pruning)
invert mctx tenv gamma sp = do
  let go = \case
        SNil -> pure (0, mempty, mempty, [])
        SApp sp (force mctx tenv -> VVar (Level x)) -> do
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

type P = StateT MetaCtx Maybe

newMeta :: MetaCtx -> Value -> (MetaVar, MetaCtx)
newMeta mctx ~mty = do
  let m' = mctx.nextMeta
      mctx' =
        mctx
          { nextMeta = mctx.nextMeta + 1,
            metaCtx = IM.insert (coerce m') (Unsolved mty) mctx.metaCtx
          }
  (m', mctx')

newMetaP :: Value -> P MetaVar
newMetaP ~mty = state (`newMeta` mty)

forceP :: TopEnv -> Value -> P Value
forceP tenv t = gets \mctx -> force mctx tenv t

evalP :: TopEnv -> Env -> Term -> P Value
evalP tenv env t = gets \mctx -> eval mctx tenv env t

lookupUnsolved :: MetaVar -> P Value
lookupUnsolved m = gets \mctx -> case mctx.metaCtx IM.! coerce m of
  Unsolved a -> a
  Solved {} -> error "lookupUnsolved"

writeMeta :: MetaVar -> Value -> Value -> P ()
writeMeta m t ~a = modify' \mctx ->
  mctx {metaCtx = IM.insert (coerce m) (Solved t a) mctx.metaCtx}

-- | Remove some arguments from a closed iterated Pi type.
pruneType :: TopEnv -> RevPruning -> Value -> P Term
pruneType tenv (RevPruning pr) a =
  go pr (PRen Nothing 0 0 mempty) a
  where
    go pr pren a = do
      a <- forceP tenv a
      case (pr, a) of
        ([], a) -> renameP tenv pren a
        (True : pr, VPi x a b) ->
          Pi x
            <$> renameP tenv pren a
            <*> go pr (liftPren pren) (b $ VVar pren.cod)
        (False : pr, VPi _ _ b) ->
          go pr (skipPren pren) (b $ VVar pren.cod)
        _ -> empty

-- | Prune arguments from a meta, return new meta + pruned type.
pruneMeta :: TopEnv -> Pruning -> MetaVar -> P MetaVar
pruneMeta tenv pr m = do
  mty <- lookupUnsolved m
  prunedty <- evalP tenv [] =<< pruneType tenv (revPruning pr) mty
  m' <- newMetaP prunedty
  solution <- evalP tenv [] =<< lams tenv (Level $ length pr) mty (AppPruning (Meta m') pr)
  writeMeta m solution mty
  pure m'

data SpinePruneStatus
  = -- | Valid spine which is a renaming
    OKRenaming
  | -- | Valid spine but not a renaming (has a non-var entry)
    OKNonRenaming
  | -- | A spine which is a renaming and has out-of-scope var entries
    NeedsPruning

-- | Prune illegal var occurrences from a meta + spine.
--   Returns: renamed + pruned term.
pruneVFlex :: TopEnv -> PartialRenaming -> MetaVar -> Spine -> P Term
pruneVFlex tenv pren m sp = do
  (sp :: [Maybe Term], status :: SpinePruneStatus) <- do
    let go = \case
          SNil -> pure ([], OKRenaming)
          SApp sp t -> do
            (sp, status) <- go sp
            forceP tenv t >>= \case
              VVar x -> case (IM.lookup (coerce x) pren.ren, status) of
                (Just x, _) -> pure (Just (Var (levelToIndex pren.dom x)) : sp, status)
                (Nothing, OKNonRenaming) -> empty
                (Nothing, _) -> pure (Nothing : sp, NeedsPruning)
              t -> case status of
                NeedsPruning -> empty
                _ -> do
                  t <- renameP tenv pren t
                  pure (Just t : sp, OKNonRenaming)
          _ -> empty
    go sp

  m' <- case status of
    OKRenaming -> pure m
    OKNonRenaming -> pure m
    NeedsPruning -> pruneMeta tenv (isJust <$> sp) m

  let t = foldr (\mu t -> maybe t (App t) mu) (Meta m') sp
  pure t

rename :: MetaCtx -> TopEnv -> PartialRenaming -> Value -> Maybe (Term, MetaCtx)
rename mctx tenv pren t = flip runStateT mctx $ renameP tenv pren t

renameP :: TopEnv -> PartialRenaming -> Value -> P Term
renameP tenv pren t =
  forceP tenv t >>= \case
    VFlex m' sp -> case pren.occ of
      Just m | m == m' -> empty -- occurs check
      _ -> pruneVFlex tenv pren m' sp
    VRigid (Level x) sp -> case IM.lookup x pren.ren of
      Nothing -> empty -- scope error ("escaping variable" error)
      Just x' -> renameSpine tenv pren (Var $ levelToIndex pren.dom x') sp
    VTop x sp -> renameSpine tenv pren (Top x) sp
    VTopAmb x sp -> renameSpine tenv pren (TopAmb x) sp
    VU -> pure U
    VPi x a b ->
      Pi x
        <$> renameP tenv pren a
        <*> renameP tenv (liftPren pren) (b $ VVar pren.cod)
    VLam x t ->
      Lam x <$> renameP tenv (liftPren pren) (t $ VVar pren.cod)
    VSigma x a b ->
      Sigma x
        <$> renameP tenv pren a
        <*> renameP tenv (liftPren pren) (b $ VVar pren.cod)
    VPair t u ->
      Pair <$> renameP tenv pren t <*> renameP tenv pren u
    VBrave {} -> empty

renameSpine :: TopEnv -> PartialRenaming -> Term -> Spine -> P Term
renameSpine tenv pren t = \case
  SNil -> pure t
  SApp sp u -> App <$> renameSpine tenv pren t sp <*> renameP tenv pren u
  SProj1 sp -> Proj1 <$> renameSpine tenv pren t sp
  SProj2 sp -> Proj2 <$> renameSpine tenv pren t sp

-- | Wrap a term in Level number of lambdas. We get the domain info from the Value
--   argument.
lams :: TopEnv -> Level -> Value -> Term -> P Term
lams tenv l a t = join $ gets \mctx -> do
  let go _ (l' :: Level) | l' == l = pure t
      go a l' = case force mctx tenv a of
        VPi "_" _ b ->
          Lam (fromString $ "x" ++ show l')
            <$> go (b $ VVar l') (l' + 1)
        VPi x _ b ->
          Lam x <$> go (b $ VVar l') (l' + 1)
        _ -> empty
  go a (0 :: Level)

-- | Solve @Γ ⊢ m spine =? rhs@.
solve :: MetaCtx -> TopEnv -> Level -> MetaVar -> Spine -> Value -> Maybe MetaCtx
solve mctx tenv gamma m sp rhs = do
  pren <- invert mctx tenv gamma sp
  solveWithPren mctx tenv m pren rhs

-- | Solve m given the result of inversion on a spine.
solveWithPren ::
  MetaCtx -> TopEnv -> MetaVar -> (PartialRenaming, Maybe Pruning) -> Value -> Maybe MetaCtx
solveWithPren mctx tenv m (pren, pruneNonLinear) rhs = flip execStateT mctx do
  mty <- lookupUnsolved m
  -- if the spine was non-linear, we check that the non-linear arguments
  -- can be pruned from the meta type (i.e. that the pruned solution will
  -- be well-typed)
  case pruneNonLinear of
    Nothing -> pure ()
    Just pr -> void $ pruneType tenv (revPruning pr) mty
  rhs <- renameP tenv (pren {occ = Just m}) rhs
  solution <- evalP tenv [] =<< lams tenv pren.dom mty rhs
  writeMeta m solution mty

--------------------------------------------------------------------------------

unify0 :: MetaCtx -> TopEnv -> Term -> Term -> [MetaCtx]
unify0 mctx tenv t t' = do
  let v = eval mctx tenv [] t
      v' = eval mctx tenv [] t'
  unify mctx tenv 0 v v'

unify :: MetaCtx -> TopEnv -> Level -> Value -> Value -> [MetaCtx]
unify mctx tenv l t t' = case (force mctx tenv t, force mctx tenv t') of
  (VBrave {}, _) -> []
  (_, VBrave {}) -> []
  (VPi _ a b, VPi _ a' b') -> do
    mctx <- unify mctx tenv l a a'
    unify mctx tenv (l + 1) (b $ VVar l) (b' $ VVar l)
  (VU, VU) -> pure mctx
  (VLam _ t, VLam _ t') ->
    unify mctx tenv (l + 1) (t $ VVar l) (t' $ VVar l)
  (t, VLam _ t') ->
    unify mctx tenv (l + 1) (t $$ VVar l) (t' $ VVar l)
  (VLam _ t, t') ->
    unify mctx tenv (l + 1) (t $ VVar l) (t' $$ VVar l)
  (VSigma _ a b, VSigma _ a' b') -> do
    mctx <- unify mctx tenv l a a'
    unify mctx tenv (l + 1) (b $ VVar l) (b' $ VVar l)
  (VPair t u, VPair t' u') -> do
    mctx <- unify mctx tenv l t t'
    unify mctx tenv l u u'
  (VPair t u, t') -> do
    mctx <- unify mctx tenv l t (vProj1 t')
    unify mctx tenv l u (vProj2 t')
  (t, VPair t' u') -> do
    mctx <- unify mctx tenv l (vProj1 t) t'
    unify mctx tenv l (vProj2 t) u'
  (VRigid x sp, VRigid x' sp')
    | x == x' -> unifySpine mctx tenv l sp sp'
  (VTop x sp, VTop x' sp')
    | x == x' -> unifySpine mctx tenv l sp sp'
  (VTop x sp, VTopAmb x' sp') ->
    asum $
      ( do
          guard $ x `S1.member` lookupResol mctx x'
          unifySpine (resolve mctx x' x) tenv l sp sp'
      )
        : [ unify mctx' tenv l (VTop x sp) t
          | (mctx', t) <- expandTopAmb mctx tenv x' sp'
          ]
  (VTopAmb x sp, VTop x' sp') ->
    asum $
      ( do
          guard $ x' `S1.member` lookupResol mctx x
          unifySpine (resolve mctx x x') tenv l sp sp'
      )
        : [ unify mctx' tenv l t (VTop x' sp')
          | (mctx', t) <- expandTopAmb mctx tenv x sp
          ]
  (VTopAmb x sp, VTopAmb x' sp')
    | x == x' -> unifySpine mctx tenv l sp sp'
  (VFlex m sp, VFlex m' sp')
    | m == m' -> unifySpine mctx tenv l sp sp'
  (VFlex m sp, t') -> maybeToList $ solve mctx tenv l m sp t'
  (t, VFlex m' sp') -> maybeToList $ solve mctx tenv l m' sp' t
  _ -> []

expandTopAmb :: MetaCtx -> TopEnv -> PQName -> Spine -> [(MetaCtx, Value)]
expandTopAmb mctx tenv x sp =
  [ (mctx', v)
  | x' <- toList $ lookupResol mctx x,
    Just t <- pure $ ML.lookup x' tenv,
    let mctx' = resolve mctx x x'
        v = vAppSpine t sp
  ]

unifySpine :: MetaCtx -> TopEnv -> Level -> Spine -> Spine -> [MetaCtx]
unifySpine mctx tenv l = \cases
  SNil SNil -> pure mctx
  (SApp sp t) (SApp sp' t') -> do
    mctx <- unifySpine mctx tenv l sp sp'
    unify mctx tenv l t t'
  (SProj1 sp) (SProj1 sp') -> unifySpine mctx tenv l sp sp'
  (SProj2 sp) (SProj2 sp') -> unifySpine mctx tenv l sp sp'
  _ _ -> []

--------------------------------------------------------------------------------
-- Rewriting types

-- | Pick up a domain without breaking dependencies.
pickUpDomain :: MetaCtx -> Ctx -> Quant -> [(Quant, Iso, MetaCtx)]
pickUpDomain mctx ctx (Quant x a b) = (Quant x a b, Refl, mctx) : go ctx.level b
  where
    go l c = case force mctx ctx.topEnv $ c (VVar l) of
      VPi y c1 c2 ->
        ( do
            let i = l - ctx.level
            -- Strengthen c1. This may involve pruning.
            (c1, mctx) <- maybeToList $ rename mctx ctx.topEnv (skipPrenN (i + 1) ctx.idRen) c1
            let c1' = eval mctx ctx.topEnv ctx.env c1
                rest ~vc1 = VPi x a (instPiAt i vc1 . b)
                s = swaps i
            pure (Quant y c1' rest, s, mctx)
        )
          ++ go (l + 1) c2
      _ -> []

    instPiAt i ~v t = case (i, force mctx ctx.topEnv t) of
      (0, VPi _ _ b) -> b v
      (i, VPi x a b) -> VPi x a (instPiAt (i - 1) v . b)
      _ -> impossible "pickUpDomain.instPiAt"

    swaps = \case
      0 -> PiSwap
      n -> piCongR (swaps (n - 1)) <> PiSwap

-- | Pick up a projection without breaking dependencies.
pickUpProjection :: MetaCtx -> Ctx -> Quant -> [(Quant, Iso, MetaCtx)]
pickUpProjection mctx ctx (Quant x a b) = (Quant x a b, Refl, mctx) : go ctx.level b
  where
    go l c = case force mctx ctx.topEnv $ c (VVar l) of
      VSigma y c1 c2 ->
        ( do
            let i = l - ctx.level
            -- Strengthen c1. This may involve pruning.
            (c1, mctx) <- maybeToList $ rename mctx ctx.topEnv (skipPrenN (i + 1) ctx.idRen) c1
            let c1' = eval mctx ctx.topEnv ctx.env c1
                rest ~vc1 = VSigma x a (instSigmaAt i vc1 . b)
                s = swaps SigmaSwap i
            pure (Quant y c1' rest, s, mctx)
        )
          ++ go (l + 1) c2
      c -> do
        let i = l - l
        (c, mctx) <- maybeToList $ rename mctx ctx.topEnv (skipPrenN (i + 1) ctx.idRen) c
        let c' = eval mctx ctx.topEnv ctx.env c
            rest ~_ = dropLastProj (l + 1) (VSigma x a b)
            s = swaps Comm i
        pure (Quant "_" c' rest, s, mctx)

    instSigmaAt i ~v t = case (i, force mctx ctx.topEnv t) of
      (0, VSigma _ _ b) -> b v
      (i, VSigma x a b) -> VSigma x a (instSigmaAt (i - 1) v . b)
      _ -> impossible "pickUpProjection.instSigmaAt"

    dropLastProj l t = case force mctx ctx.topEnv t of
      VSigma x a b -> case b (VVar l) of
        VSigma {} -> VSigma x a (dropLastProj (l + 1) . b)
        _ -> a
      _ -> impossible "pickUpProjection.dropLastProj"

    swaps i = \case
      0 -> i
      n -> sigmaCongR (swaps i (n - 1)) <> SigmaSwap

-- | Pick a **non-sigma** projection without breaking dependencies.
-- This works even in the presence of arbitrarily nested sigmas in the type.
assocSwap :: MetaCtx -> Ctx -> Quant -> [(Quant, Iso, MetaCtx)]
assocSwap mctx ctx q = do
  -- Pick one projection first.
  (q, i, mctx) <- pickUpProjection mctx ctx q
  case q of
    -- When the selected projection is a sigma type, we invoke
    -- assocSwap recursively to make the first projection of the sigma non-sigma!
    Quant x (VSigma y a b) c -> do
      (Quant y a b, j, mctx) <- assocSwap mctx ctx (Quant y a b)
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
currySwap :: MetaCtx -> Ctx -> Quant -> [(Quant, Iso, MetaCtx)]
currySwap mctx ctx q = do
  (q, i, mctx) <- pickUpDomain mctx ctx q
  case q of
    Quant x (VSigma y a b) c -> do
      (Quant y a b, j, mctx) <- assocSwap mctx ctx (Quant y a b)
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
  (i, i', mctx) <- unifyIso mctx (initCtx tenv) v v'
  let j = i <> sym i'
  pure (j, mctx)

unifyIso :: MetaCtx -> Ctx -> Value -> Value -> [(Iso, Iso, MetaCtx)]
unifyIso mctx ctx t u = case (force mctx ctx.topEnv t, force mctx ctx.topEnv u) of
  (VBrave {}, _) -> []
  (_, VBrave {}) -> []
  (VPi x a b, VPi x' a' b') ->
    unifyPi mctx ctx (Quant x a b) (Quant x' a' b')
  (VSigma x a b, VSigma x' a' b') ->
    unifySigma mctx ctx (Quant x a b) (Quant x' a' b')
  (t, u) -> do
    mctx <- unify mctx ctx.topEnv ctx.level t u
    pure (Refl, Refl, mctx)

unifySpineRefl :: MetaCtx -> Ctx -> Spine -> Spine -> [(Iso, Iso, MetaCtx)]
unifySpineRefl mctx ctx sp sp' = do
  mctx <- unifySpine mctx ctx.topEnv ctx.level sp sp'
  pure (Refl, Refl, mctx)

unifyPi :: MetaCtx -> Ctx -> Quant -> Quant -> [(Iso, Iso, MetaCtx)]
unifyPi mctx ctx q q' = do
  let (Quant x a b, i) = curry mctx ctx.topEnv q
  flip foldMapA (currySwap mctx ctx q') \(Quant _ a' b', i', mctx) -> do
    (ia, ia', mctx) <- unifyIso mctx ctx a a'
    let v = transportInv ia (VVar ctx.level)
        v' = transportInv ia' (VVar ctx.level)
    (ib, ib', mctx) <- unifyIso mctx (bind mctx ctx x VU) (b v) (b' v')
    let j = i <> piCongL ia <> piCongR ib
        j' = i' <> piCongL ia' <> piCongR ib'
    pure (j, j', mctx)

unifySigma :: MetaCtx -> Ctx -> Quant -> Quant -> [(Iso, Iso, MetaCtx)]
unifySigma mctx ctx q q' = do
  let (Quant x a b, i) = assoc mctx ctx.topEnv q
  flip foldMapA (assocSwap mctx ctx q') \(Quant _ a' b', i', mctx) -> do
    (ia, ia', mctx) <- unifyIso mctx ctx a a'
    let v = transportInv ia (VVar ctx.level)
        v' = transportInv ia' (VVar ctx.level)
    (ib, ib', mctx) <- unifyIso mctx (bind mctx ctx x VU) (b v) (b' v')
    let j = i <> sigmaCongL ia <> sigmaCongR ib
        j' = i' <> sigmaCongL ia' <> sigmaCongR ib'
    pure (j, j', mctx)

--------------------------------------------------------------------------------

closeTy :: Locals -> Term -> Term
closeTy = \cases
  Here b -> b
  (Bind locs x a) b -> closeTy locs (Pi x a b)

closeTm :: Locals -> Term -> Term
closeTm = \cases
  Here t -> t
  (Bind locs x _) b -> closeTm locs (Lam x b)

-- FIXME: currently not considering pi permutation
check :: QName -> Ctx -> M.Map PQName (S1.NESet QName) -> Value -> Value -> IStr.Stream (Iso, Term)
check h ctx resol query item =
  ( do
      (item, inst, mctx) <- possibleInstantiation (emptyMetaCtx resol) ctx item (VTop h SNil)
      (i, i', mctx) <- IStr.maybeToStream $ listToMaybe $ unifyIso mctx ctx query item
      guard $ allMetaSolved mctx
      let j = i <> sym i'
          ~sol = closeTm ctx.locals $ quote mctx ctx.topEnv ctx.level $ transportInv j inst
      pure (j, sol)
  )
    <|> IStr.Later case force (emptyMetaCtx resol) ctx.topEnv query of
      VPi "_" _ _ -> empty
      VPi x a b -> do
        check h (bind (emptyMetaCtx resol) ctx x a) resol (b $ VVar ctx.level) item
      _ -> empty

freshMeta :: MetaCtx -> Ctx -> Value -> (Term, MetaCtx)
freshMeta mctx ctx a = do
  let ~closed = eval mctx ctx.topEnv [] $ closeTy ctx.locals (quote mctx ctx.topEnv ctx.level a)
      (m, mctx') = newMeta mctx closed
  (AppPruning (Meta m) (replicate (coerce ctx.level) True), mctx')

-- FIXME: currently not considering pi permutation
possibleInstantiation :: MetaCtx -> Ctx -> Value -> Value -> IStr.Stream (Value, Value, MetaCtx)
possibleInstantiation mctx ctx a ~inst =
  pure (a, inst, mctx)
    <|> IStr.Later case force mctx ctx.topEnv a of
      VPi "_" _ _ -> empty
      VPi _ a b -> do
        (m, mctx) <- pure $ freshMeta mctx ctx a
        let mv = eval mctx ctx.topEnv ctx.env m
        possibleInstantiation mctx ctx (b mv) (inst $$ mv)
      _ -> empty
