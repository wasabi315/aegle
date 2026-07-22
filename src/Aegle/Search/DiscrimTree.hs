module Aegle.Search.DiscrimTree where

import Aegle.Core.Name
import Aegle.Prelude
import Data.Map.Lazy qualified as ML
import Prettyprinter

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
data DiscrimTree a
  = Leaf a
  | Node (ML.Map Token (DiscrimTree a))
  deriving stock (Functor, Foldable, Traversable)

one :: Token -> DiscrimTree a -> DiscrimTree a
one tok ~dt = Node $ ML.singleton tok dt

extract :: DiscrimTree a -> a
extract = \case
  Leaf x -> x
  Node {} -> impossible "extract"

children :: DiscrimTree a -> [(Token, DiscrimTree a)]
children = \case
  Leaf {} -> impossible "children"
  Node dts -> ML.toAscList dts

child :: Token -> DiscrimTree a -> Maybe (DiscrimTree a)
child tok = \case
  Leaf {} -> impossible "child"
  Node dts -> ML.lookup tok dts

--------------------------------------------------------------------------------

union :: (Semigroup a) => DiscrimTree a -> DiscrimTree a -> DiscrimTree a
union = \cases
  (Leaf x) (Leaf y) -> Leaf $ x <> y
  (Node dts) (Node dts') -> Node $ ML.unionWith union dts dts'
  (Leaf {}) (Node {}) -> impossible "unionWith"
  (Node {}) (Leaf {}) -> impossible "unionWith"

instance (Semigroup a) => Semigroup (DiscrimTree a) where
  (<>) = union
  {-# INLINE (<>) #-}

instance (Semigroup a) => Monoid (DiscrimTree a) where
  mempty = Node mempty
  {-# INLINE mempty #-}

--------------------------------------------------------------------------------
-- Prettyprinting for debugging

instance Pretty Token where
  pretty = \case
    TRigid x len -> pretty x <> "/" <> pretty len
    TOpaque x len -> pretty x <> "/" <> pretty len
    TU -> "U"
    TPi -> "Π"
    TLam -> "λ"
    TSigma -> "Σ"
    TPair -> ","
    TApp -> "@"
    TProj1 -> ".1"
    TProj2 -> ".2"
    TEtaLam -> "ηλ"
    TEtaPair -> "η,"

instance (Pretty a) => Pretty (DiscrimTree a) where
  pretty = \case
    Leaf x -> "ε →" <+> align (pretty x)
    Node (notEtas -> dts)
      | [] <- dts -> "∅"
      | otherwise -> vsep $ uncurry go <$> dts
    where
      go tok = go' (pretty tok :)

      go' toks = \case
        Leaf x -> hsep (toks []) <> " →" <+> align (pretty x)
        Node dts -> case notEtas dts of
          [(tok, dt)] -> go' (toks . (pretty tok :)) dt
          [] -> hsep (toks []) <> ": ∅"
          dts ->
            hsep (toks []) <> ":" <> nest 2 do
              line <> vsep (uncurry go <$> dts)

      notEtas = filter ((`notElem` [TEtaLam, TEtaPair]) . fst) . ML.toAscList
