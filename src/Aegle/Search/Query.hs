module Aegle.Search.Query
  ( Query (..),
    Term (..),
    Type,
    teleView,
    endsInSort,
    appView,
    headTerm,
    returnType,
    freeVars,
    toTerm,
    Unqualified (..),
  )
where

import Aegle.Core.Name
import Aegle.Core.Term (AppView (..), TeleView (..))
import Aegle.Core.Term qualified as C
import Aegle.Prelude
import Data.Set qualified as S
import Data.Text qualified as T
import Prettyprinter

--------------------------------------------------------------------------------

data Query = Query
  { names :: [T.Text],
    typ :: Type
  }
  deriving stock (Show)

-- | Query term
data Term
  = Var PQName
  | U
  | Pi Name Term Term
  | Lam Name Term
  | App Term Term
  | Sigma Name Term Term
  | Pair Term Term
  | Proj1 Term
  | Proj2 Term
  deriving stock (Show)

type Type = Term

--------------------------------------------------------------------------------

teleView :: Term -> TeleView Term
teleView = go []
  where
    go tele = \case
      Pi x a b -> go ((x, a) : tele) b
      cod -> TeleView tele cod

-- | Get the return type. Doesn't perform any reduction.
returnType :: Term -> Term
returnType = (.cod) . teleView

-- | Is the return type U? Doesn't perform any reduction.
endsInSort :: Term -> Bool
endsInSort t = case returnType t of
  U -> True
  _ -> False

appView :: Term -> AppView Term
appView = go id
  where
    go args = \case
      App t u -> go ((u :) . args) t
      t -> AppView {head = t, args = args []}

-- | The head term. Doesn't consider projections as elimination. Doesn't perform any reduction.
headTerm :: Term -> Term
headTerm = (.head) . appView

freeVars :: Term -> S.Set PQName
freeVars = \case
  Var x -> S.singleton x
  U -> S.empty
  Pi x a b -> S.delete (Unqual x) $ freeVars a <> freeVars b
  Lam x t -> S.delete (Unqual x) $ freeVars t
  App t u -> freeVars t <> freeVars u
  Sigma x a b -> S.delete (Unqual x) $ freeVars a <> freeVars b
  Pair t u -> freeVars t <> freeVars u
  Proj1 t -> freeVars t
  Proj2 t -> freeVars t

toTerm :: Term -> C.Term
toTerm = go []
  where
    go ns = \case
      Var (Unqual x)
        | Just i <- x `elemIndex` ns -> C.Var (Index i)
      Var x -> C.TopAmb x
      U -> C.U
      Pi x a b -> do
        let x' = if Unqual x `S.member` freeVars b then x else "_"
        C.Pi x' (go ns a) (go (x' : ns) b)
      Lam x t -> C.Lam x (go (x : ns) t)
      App t u -> C.App (go ns t) (go ns u)
      Sigma x a b -> do
        let x' = if Unqual x `S.member` freeVars b then x else "_"
        C.Sigma x' (go ns a) (go (x' : ns) b)
      Pair t u -> C.Pair (go ns t) (go ns u)
      Proj1 t -> C.Proj1 (go ns t)
      Proj2 t -> C.Proj2 (go ns t)

--------------------------------------------------------------------------------
-- Prettyprinting

newtype Unqualified a = Unqualified a

instance Pretty Query where
  pretty Query {..} =
    hsep (pretty <$> names) <+> colon <+> pretty typ

instance Pretty (Unqualified Query) where
  pretty (Unqualified Query {..}) =
    hsep (pretty <$> names) <+> colon <+> pretty (Unqualified typ)

instance Pretty Term where
  pretty = pretty' True

instance Pretty (Unqualified Term) where
  pretty = pretty' False . coerce

pretty' :: Bool -> Term -> Doc ann
pretty' qual = goPair
  where
    goPair = \case
      Pair t u -> goLam t <+> comma <+> goPair u
      t -> goLam t

    goLam = \case
      Lam n t -> do
        let go = \case
              Lam n t -> " " <> pretty n <> go t
              t -> "." <+> goLam t
        "λ" <+> pretty n <> go t
      t -> goPi t

    goPi = \case
      Pi "_" a b -> goSigma a <+> "→" <+> goPi b
      Pi n a b -> do
        let go = \case
              Pi "_" a b -> " →" <+> goSigma a <+> "→" <+> goPi b
              Pi n a b -> " " <> piBind n a <> go b
              b -> " →" <+> goPi b
        piBind n a <> go b
      t -> goSigma t

    goSigma = \case
      Sigma "_" a b -> goApp a <+> "×" <+> goSigma b
      Sigma n a b -> piBind n a <+> "×" <+> goSigma b
      t -> goApp t

    goApp = \case
      App t u -> goApp t <+> goProj u
      t -> goProj t

    goProj = \case
      Proj1 t -> goProj t <> ".1"
      Proj2 t -> goProj t <> ".2"
      t -> goAtom t

    goAtom = \case
      Var (Unqual n) -> pretty n
      Var (Qual m n)
        | qual -> pretty (Qual m n)
        | otherwise -> pretty n
      U -> "Set"
      t -> parens (goPair t)

    piBind n a = parens $ pretty n <+> colon <+> goPair a
