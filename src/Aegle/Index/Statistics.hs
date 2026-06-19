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
import Data.Generics.Labels ()
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
  numItem <- Foldl.handles ( #definitions . Foldl.folded) Foldl.length
  numItemPerFeatureShape <-
    Foldl.handles ( #definitions . Foldl.folded . #feature) do
      Foldl.groupBy toFeatureShape Foldl.length
  numItemPerArity <-
    Foldl.handles ( #definitions . Foldl.folded . #feature . #arity) do
      Foldl.groupBy id Foldl.length
  numItemPerRHTop <-
    Foldl.handles ( #definitions . Foldl.folded . #feature . #resultHead . #_RHTop) do
      Foldl.groupBy id Foldl.length
  numCanonicalNamePerUnqualName <-
      Foldl.handles ( #exports . Foldl.folded) do
        Foldl.groupBy (.exportAs.name) do
          dimap (.canonicalName) S.size Foldl.set
  pure Statistics {..}

toFeatureShape :: AllFeature n -> FeatureShape
toFeatureShape AllFeature {..} =
  FeatureShape
    { resultHead = void resultHead,
      polymorphic,
      arityHasVar = arity.hasVar
    }
