module Aegle.Core.Term
  ( Term (..),
    Type,
    Pruning,
    RevPruning (..),
    revPruning,
    freeVars,
    subst,
    rename,
    weakenBy,
    TeleView (..),
    teleView,
    returnType,
    endsInSort,
    AppView (..),
    appView,
    headTerm,
    termSize,
    Unqualified (..),
    subscript,
  )
where

import Aegle.Core.Name
import Aegle.Prelude
import Data.Set qualified as S
import Data.Text qualified as T
import Flat
import Prettyprinter

--------------------------------------------------------------------------------

-- | Terms
data Term
  = Var Index -- x
  | Meta MetaVar -- ?m
  | Top {-# UNPACK #-} QName -- M.f
  | TopAmb PQName
  | U -- U
  | Pi Name Type Type -- (x : A) → B
  | Lam Name Term -- λ x → t
  | App Term Term -- t1 t2
  | AppPruning Term Pruning
  | Sigma Name Type Type -- (x : A) × B
  | Pair Term Term -- (t1, t2)
  | Proj1 Term -- t.1
  | Proj2 Term -- t.2
  deriving stock (Show, Generic)
  deriving anyclass (Flat, NFData)

type Type = Term

type Pruning = [Bool]

newtype RevPruning = RevPruning Pruning

revPruning :: Pruning -> RevPruning
revPruning = RevPruning . reverse

--------------------------------------------------------------------------------

freeVars :: Term -> S.Set Index
freeVars = \case
  Var i -> S.singleton i
  Meta {} -> S.empty
  Top {} -> S.empty
  TopAmb {} -> S.empty
  U -> S.empty
  Pi _ a b -> freeVars a <> freeVarBind b
  Lam _ t -> freeVarBind t
  App t u -> freeVars t <> freeVars u
  Sigma _ a b -> freeVars a <> freeVarBind b
  Pair t u -> freeVars t <> freeVars u
  Proj1 t -> freeVars t
  Proj2 t -> freeVars t
  AppPruning {} -> error "freeVar: AppPruning is not supported yet"
  where
    freeVarBind t = S.mapMonotonic (subtract 1) $ S.filter (> 0) $ freeVars t

subst :: (Index -> Term) -> Term -> Term
subst s = \case
  Var i -> s i
  t@Meta {} -> t
  t@Top {} -> t
  t@TopAmb {} -> t
  U -> U
  Pi x a b -> Pi x (subst s a) (substBind s b)
  Lam x t -> Lam x (substBind s t)
  App t u -> App (subst s t) (subst s u)
  Sigma x a b -> Sigma x (subst s a) (substBind s b)
  Pair t u -> Pair (subst s t) (subst s u)
  Proj1 t -> Proj1 (subst s t)
  Proj2 t -> Proj2 (subst s t)
  AppPruning {} -> error "subst: substitution on AppPruning is not supported yet"
  where
    substBind s = subst \case
      0 -> Var 0
      i -> weakenBy 1 $ s (i - 1)

rename :: (Index -> Index) -> Term -> Term
rename r = subst (Var . r)

weakenBy :: Int -> Term -> Term
weakenBy n = rename (coerce n +)

data TeleView a = TeleView
  { tele :: [(Name, a)],
    cod :: a
  }

teleView :: Type -> TeleView Type
teleView = go []
  where
    go tele = \case
      Pi x a b -> go ((x, a) : tele) b
      cod -> TeleView tele cod

-- | Get the return type. Doesn't perform any reduction.
returnType :: Type -> Type
returnType = (.cod) . teleView

-- | Is the return type U? Doesn't perform any reduction.
endsInSort :: Type -> Bool
endsInSort t = case returnType t of
  U -> True
  _ -> False

data AppView a = AppView
  { head :: a,
    args :: [a]
  }

appView :: Term -> AppView Term
appView = go id
  where
    go args = \case
      App t u -> go ((u :) . args) t
      t -> AppView {head = t, args = args []}

-- | The head term. Doesn't consider projections as elimination. Doesn't perform any reduction.
headTerm :: Term -> Term
headTerm = (.head) . appView

termSize :: Term -> Int
termSize = \case
  Var {} -> 1
  U -> 1
  Meta {} -> 1
  Top {} -> 1
  TopAmb {} -> 1
  Pi _ a b -> 1 + termSize a + termSize b
  Lam _ t -> 1 + termSize t
  App t u -> 1 + termSize t + termSize u
  AppPruning t pr -> termSize t + length (filter id pr)
  Sigma _ a b -> 1 + termSize a + termSize b
  Pair t u -> 1 + termSize t + termSize u
  Proj1 t -> 1 + termSize t
  Proj2 t -> 1 + termSize t

--------------------------------------------------------------------------------
-- Prettyprinting

instance Pretty Term where
  pretty = pretty' True []

newtype Unqualified = Unqualified Term

instance Pretty Unqualified where
  pretty = pretty' False [] . coerce

instance Pretty ([Name] ⊢ Term) where
  pretty (ns :⊢ t) = pretty' True ns t

instance Pretty ([Name] ⊢ Unqualified) where
  pretty (ns :⊢ t) = pretty' False ns (coerce t)

pretty' :: Bool -> [Name] -> Term -> Doc ann
pretty' qual = goPair
  where
    goPair ns = \case
      Pair t u -> goLam ns t <+> comma <+> goPair ns u
      t -> goLam ns t

    goLam ns = \case
      Lam (freshen ns -> n) t -> do
        let go ns = \case
              Lam (freshen ns -> n) t -> " " <> pretty n <> go (n : ns) t
              t -> "." <+> goLam ns t
        "λ" <+> pretty n <> go (n : ns) t
      t -> goPi ns t

    goPi ns = \case
      Pi "_" a b -> goSigma ns a <+> "→" <+> goPi ("_" : ns) b
      Pi (freshen ns -> n) a b -> do
        let go ns = \case
              Pi "_" a b -> " →" <+> goSigma ns a <+> "→" <+> goPi ("_" : ns) b
              Pi (freshen ns -> n) a b -> " " <> piBind n ns a <> go (n : ns) b
              b -> " →" <+> goPi ns b
        piBind n ns a <> go (n : ns) b
      t -> goSigma ns t

    goSigma ns = \case
      Sigma "_" a b -> goApp ns a <+> "×" <+> goSigma ("_" : ns) b
      Sigma (freshen ns -> n) a b -> piBind n ns a <+> "×" <+> goSigma (n : ns) b
      t -> goApp ns t

    goApp ns = \case
      App t u -> goApp ns t <+> goProj ns u
      AppPruning t pr -> do
        let go t i = \cases
              _ [] -> t
              (n : ns) (True : pr) -> go t i ns pr <+> pretty n
              (_ : ns) (False : pr) -> go t i ns pr
              [] (True : pr) -> go t (i + 1) [] pr <+> pretty @Index i
              [] (False : pr) -> go t (i + 1) [] pr
        go (goApp ns t) 0 ns pr
      t -> goProj ns t

    goProj ns = \case
      Proj1 t -> goProj ns t <> ".1"
      Proj2 t -> goProj ns t <> ".2"
      t -> goAtom ns t

    goAtom ns = \case
      Var i -> case ns !? coerce i of
        Nothing -> pretty i
        Just n -> pretty n
      Meta m -> pretty m
      Top x
        | qual -> pretty x
        | otherwise -> pretty x.name
      TopAmb (Unqual x) -> pretty x
      TopAmb (Qual m x)
        | qual -> pretty (Qual m x)
        | otherwise -> pretty x
      U -> "U"
      t -> parens (goPair ns t)

    piBind n ns a = parens $ pretty n <+> colon <+> goPair ns a

freshen :: [Name] -> Name -> Name
freshen ns n
  | n `elem` ns = go 0
  | otherwise = n
  where
    go (i :: Int)
      | n' `notElem` ns = n'
      | otherwise = go (i + 1)
      where
        n' = Name $ coerce n <> T.map subscript (T.show i)

subscript :: Char -> Char
subscript = \case
  '0' -> '₀'
  '1' -> '₁'
  '2' -> '₂'
  '3' -> '₃'
  '4' -> '₄'
  '5' -> '₅'
  '6' -> '₆'
  '7' -> '₇'
  '8' -> '₈'
  '9' -> '₉'
  c -> c
