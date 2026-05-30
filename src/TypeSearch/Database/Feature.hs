module TypeSearch.Database.Feature where

import Data.Set qualified as S
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Database.Query qualified as Q
import TypeSearch.Prelude

--------------------------------------------------------------------------------

class Feature a where
  compatible :: "query" :! a -> "db" :! a -> Bool
  compatible queryFeat = matchesCompat (toCompat queryFeat)

  -- | Reified compatibility condition produced from a query feature.
  -- Backend code can compile this e.g. to SQL.
  type Compat a

  toCompat :: "query" :! a -> Compat a
  matchesCompat :: Compat a -> "db" :! a -> Bool

--------------------------------------------------------------------------------
-- Features

data ReturnTypeHead n
  = RHU
  | RHVar
  | RHTop n
  | RHSigma
  | RHProj1
  | RHProj2
  | RHUnknown
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (ToJSON, FromJSON)

-- | The input type must be closed. Doesn't perform any reduction.
returnTypeHead :: Type -> ReturnTypeHead QName
returnTypeHead t = case headTerm (returnType t) of
  U -> RHU
  Var {} -> RHVar
  Top x -> RHTop x
  Sigma {} -> RHSigma
  Proj1 {} -> RHProj1
  Proj2 {} -> RHProj2
  _ -> impossible

-- | The input type must be closed.
returnTypeHeadQ :: S.Set QName -> Q.Type -> Maybe (ReturnTypeHead PQName)
returnTypeHeadQ transparentDefNames (Q.teleView -> TeleView tele cod) =
  case Q.headTerm cod of
    Q.U -> Just RHU
    Q.Var (Unqual x)
      | Just {} <- lookup x tele -> Just RHVar
    Q.Var x | maybeTransparentDef x -> Just RHUnknown
    Q.Var x -> Just $ RHTop x
    Q.Sigma {} -> Just RHSigma
    Q.Proj1 {} -> Just RHProj1
    Q.Proj2 {} -> Just RHProj2
    _ -> Nothing
  where
    maybeTransparentDef = \case
      Unqual x -> any (\y -> x == y.name) transparentDefNames
      Qual m x -> S.member (QName m x) transparentDefNames

data ReturnTypeHeadCompat n
  = IsVar
  | IsVarOrU
  | IsVarOrTop n
  | IsVarOrSigma
  | IsVarOrProj1
  | IsVarOrProj2
  | AnyReturnType
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

instance (Eq n) => Feature (ReturnTypeHead n) where
  type Compat (ReturnTypeHead n) = ReturnTypeHeadCompat n

  toCompat = \case
    Arg RHU -> IsVarOrU
    Arg RHVar -> IsVar
    Arg (RHTop n) -> IsVarOrTop n
    Arg RHSigma -> IsVarOrSigma
    Arg RHProj1 -> IsVarOrProj1
    Arg RHProj2 -> IsVarOrProj2
    Arg RHUnknown -> AnyReturnType

  matchesCompat = \cases
    _ (Arg RHVar) -> True
    IsVarOrU (Arg RHU) -> True
    (IsVarOrTop x) (Arg (RHTop y)) -> x == y
    IsVarOrSigma (Arg RHSigma) -> True
    IsVarOrProj1 (Arg RHProj1) -> True
    IsVarOrProj2 (Arg RHProj2) -> True
    _ _ -> False

--------------------------------------------------------------------------------

-- | Polymorphic feature.
data Polymorphic = Monomorphic | Polymorphic
  deriving stock (Eq, Ord, Show, Read, Enum)

-- | The input type must be closed. Doesn't perform any reduction.
polymorphic :: Type -> Polymorphic
polymorphic = \case
  Pi _ a _ | endsInSort a -> Polymorphic
  Pi _ _ b -> polymorphic b
  _ -> Monomorphic

-- | The input type must be closed.
polymorphicQ :: Q.Type -> Polymorphic
polymorphicQ = \case
  Q.Pi _ a _ | Q.endsInSort a -> Polymorphic
  Q.Pi _ _ b -> polymorphicQ b
  _ -> Monomorphic

data PolymorphicCompat
  = IsPoly
  | AnyPoly
  deriving stock (Show, Generic)

instance Feature Polymorphic where
  type Compat Polymorphic = PolymorphicCompat

  toCompat = \case
    (Arg Polymorphic) -> IsPoly
    (Arg Monomorphic) -> AnyPoly

  matchesCompat = \cases
    IsPoly (Arg Polymorphic) -> True
    IsPoly (Arg Monomorphic) -> False
    AnyPoly _ -> True

--------------------------------------------------------------------------------

-- | Arity feature.
data Arity = Arity
  { hasVar :: Bool,
    arity :: Int
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The input type must be closed. Doesn't perform any reduction.
arity :: Type -> Arity
arity = go [] False 0
  where
    go ctx hasVar arity = \case
      Pi _ a b -> case headTerm a of
        Var i
          | endsInSort (ctx !! coerce i) ->
              go (a : ctx) True (arity + 1) b
        _ ->
          go (a : ctx) hasVar (arity + 1) b
      _ -> Arity {..}

-- | The input type must be closed.
arityQ :: Q.Type -> Arity
arityQ = go [] False 0
  where
    go ctx hasVar arity = \case
      Q.Pi x a b -> case Q.headTerm a of
        Q.Var (Unqual y)
          | Just t <- lookup y ctx,
            Q.endsInSort t ->
              go ((x, a) : ctx) True (arity + 1) b
        _ ->
          go ((x, a) : ctx) hasVar (arity + 1) b
      _ -> Arity {..}

data ArityCompat
  = HasVar
  | HasVarOrGe Int
  deriving stock (Show, Generic)

instance Feature Arity where
  type Compat Arity = ArityCompat

  toCompat (Arg Arity {..}) =
    if hasVar then HasVar else HasVarOrGe arity

  matchesCompat comp (Arg Arity {..}) = case comp of
    HasVar -> hasVar
    HasVarOrGe arity' -> hasVar || arity >= arity'

--------------------------------------------------------------------------------

data AllFeature n = AllFeature
  { returnTypeHead :: ReturnTypeHead n,
    polymorphic :: Polymorphic,
    arity :: Arity
  }
  deriving stock (Show, Generic)

allFeature :: Type -> AllFeature QName
allFeature typ =
  AllFeature
    { returnTypeHead = returnTypeHead typ,
      polymorphic = polymorphic typ,
      arity = arity typ
    }

-- featureQ :: S.Set QName -> Q.Type -> Maybe (Feature PQName)
-- featureQ transparentDefNames typ = do
--   returnTypeHead <- returnTypeHeadQ transparentDefNames typ
--   pure
--     Feature
--       { returnTypeHead,
--         polymorphic = polymorphicQ typ,
--         arity = arityQ typ
--       }

data AllFeatureCompat n = AllFeatureCompat
  { returnTypeHead :: ReturnTypeHeadCompat n,
    polymorphic :: PolymorphicCompat,
    arity :: ArityCompat
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

instance (Eq n) => Feature (AllFeature n) where
  type Compat (AllFeature n) = AllFeatureCompat n

  toCompat (Arg AllFeature {..}) =
    AllFeatureCompat
      { returnTypeHead = toCompat (Arg returnTypeHead),
        polymorphic = toCompat (Arg polymorphic),
        arity = toCompat (Arg arity)
      }

  matchesCompat conds (Arg feats) =
    matchesCompat conds.returnTypeHead (Arg feats.returnTypeHead)
      && matchesCompat conds.polymorphic (Arg feats.polymorphic)
      && matchesCompat conds.arity (Arg feats.arity)
