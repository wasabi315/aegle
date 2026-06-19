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

--------------------------------------------------------------------------------

data Statistics = Statistics
  { numItem :: {-# UNPACK #-} Int,
    numItemPerFeature :: M.Map FeatureShape Int,
    numItemPerRHTop :: M.Map QName Int
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
  numItemPerFeature <-
    Foldl.handles ( #definitions . Foldl.folded) do
      Foldl.groupBy (toFeatureShape . (.feature)) Foldl.length
  numItemPerRHTop <-
    Foldl.handles ( #definitions . Foldl.folded . #feature . #resultHead . #_RHTop) do
      Foldl.groupBy id Foldl.length
  pure Statistics {..}

toFeatureShape :: AllFeature n -> FeatureShape
toFeatureShape AllFeature {..} =
  FeatureShape
    { resultHead = void resultHead,
      polymorphic,
      arityHasVar = arity.hasVar
    }
