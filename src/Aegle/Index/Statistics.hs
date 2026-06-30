{-# LANGUAGE ApplicativeDo #-}

module Aegle.Index.Statistics
  ( Statistics (..),
    FeatureShape (..),
    statisticsBuilder,
  )
where

import Aegle.Core.Name
import Aegle.Database.Backend
import Aegle.Prelude
import Aegle.Search.Feature
import Control.Foldl qualified as Foldl
import Data.Map.Strict qualified as M
import Data.Set qualified as S

--------------------------------------------------------------------------------

data Statistics = Statistics
  { numItem :: {-# UNPACK #-} Int,
    numItemPerFeatureShape :: M.Map FeatureShape Int,
    numItemPerArity :: M.Map Arity Int,
    numItemPerRHTop :: M.Map QName Int,
    numCanonicalNamePerUnqualName :: M.Map Name Int
  }
  deriving stock (Eq, Ord, Show, Generic)

data FeatureShape = FeatureShape
  { resultHead :: ResultHead (),
    polymorphic :: Polymorphic,
    arityHasVar :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)

--------------------------------------------------------------------------------

statisticsBuilder :: Foldl.Fold LibraryFragment Statistics
statisticsBuilder = do
  numItem <-
    countOf (#definitions . traverse)
  numItemPerFeatureShape <-
    histogramOf (#definitions . traverse . #feature . to toFeatureShape)
  numItemPerArity <-
    histogramOf (#definitions . traverse . #feature . #arity)
  numItemPerRHTop <-
    histogramOf (#definitions . traverse . #feature . #resultHead . #_RHTop)
  numCanonicalNamePerUnqualName <-
    Foldl.handles (#exports . traverse) do
      Foldl.groupBy (.exportAs.name) do
        distinctCountOf #canonicalName
  pure Statistics {..}

toFeatureShape :: AllFeature n -> FeatureShape
toFeatureShape AllFeature {..} =
  FeatureShape
    { resultHead = void resultHead,
      polymorphic,
      arityHasVar = arity.hasVar
    }

--------------------------------------------------------------------------------

countOf :: Foldl.Handler a b -> Foldl.Fold a Int
countOf h = Foldl.handles h Foldl.length

distinctCountOf :: (Ord b) => Foldl.Handler a b -> Foldl.Fold a Int
distinctCountOf h = Foldl.handles h (S.size <$> Foldl.set)

histogramOf :: (Ord b) => Foldl.Handler a b -> Foldl.Fold a (M.Map b Int)
histogramOf h = Foldl.handles h (Foldl.groupBy id Foldl.length)
