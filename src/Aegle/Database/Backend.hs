module Aegle.Database.Backend
  ( Referent (..),
    LibraryItem (..),
    DbReader (..),
    resolveNames,
    loadByAnyFeature,
    Definition (..),
    Export (..),
    LibraryFragment (..),
    DbBuilder,
  )
where

import Aegle.Core.Name
import Aegle.Core.Term
import Aegle.Prelude
import Aegle.Search.Feature
import Control.Foldl qualified as Foldl

--------------------------------------------------------------------------------
-- Build operation

data Definition = Definition
  { name :: QName,
    signature :: Type,
    originalSignature :: Type,
    feature :: AllFeature QName,
    body :: Maybe Term,
    moduleName :: ModuleName,
    position :: Int
  }
  deriving stock (Show, Generic)

data Export = Export
  { canonicalName :: QName,
    exportAs :: QName
  }
  deriving stock (Show, Generic)

data LibraryFragment = LibraryFragment
  { definitions :: [Definition],
    exports :: [Export]
  }
  deriving stock (Show, Generic)
  deriving (Semigroup, Monoid) via Generically LibraryFragment

-- | Build database by consuming library fragments.
-- Note that exports in earlier fragments may refer to canonical names defined in later ones.
type DbBuilder m = Foldl.FoldM m LibraryFragment

--------------------------------------------------------------------------------
-- Read operation

data Referent = Referent
  { canonicalName :: QName,
    body :: Maybe Term
  }
  deriving stock (Show, Generic)

data LibraryItem = LibraryItem
  { canonicalName :: QName,
    reexportedAs :: [QName],
    signature :: Type,
    originalSignature :: Type,
    moduleName :: ModuleName,
    position :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data DbReader m = DbReader
  { resolveNames ::
      forall t.
      (Traversable t) => t PQName -> m (t [Referent]),
    loadByAnyFeature ::
      forall t.
      (Foldable t) => t (Compat (AllFeature PQName)) -> m [LibraryItem]
  }

-- These accessors are provided because the record dot syntax does not work
-- for polymorphic fields.

-- | Resolve possibly-qualified names.
resolveNames :: (Traversable t) => DbReader m -> t PQName -> m (t [Referent])
resolveNames DbReader {..} = resolveNames

-- | Load library items that match at least one of the given compatibilities.
-- Note that the compatibilities may contain unresolved names.
loadByAnyFeature :: (Foldable t) => DbReader m -> t (Compat (AllFeature PQName)) -> m [LibraryItem]
loadByAnyFeature DbReader {..} = loadByAnyFeature
