module TypeSearch.Database.Backend
  ( Referent (..),
    LibraryItem (..),
    DbReader (..),
    Definition (..),
    Export (..),
    LibraryFragment (..),
    DbBuilder (..),
  )
where

import ListT qualified
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Database.Feature
import TypeSearch.Prelude

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
    signature :: Type
  }
  deriving stock (Show, Generic)

data DbReader m = DbReader
  { -- | Resolve possibly-qualified names.
    resolveNames ::
      forall t.
      (Traversable t) => t PQName -> m (t [Referent]),
    -- | Load library items that match at least one of the given features.
    loadByAnyFeature ::
      forall t.
      (Foldable t) => t (Feature QName) -> ListT.ListT m LibraryItem
  }

--------------------------------------------------------------------------------
-- Build operation

data Definition = Definition
  { name :: QName,
    signature :: Type,
    feature :: Feature QName,
    body :: Maybe Term
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

newtype DbBuilder m = DbBuilder
  { -- | Build database from a stream of library fragments.
    -- Note that re-exports in earlier fragments may refer to canonical names defined in later ones.
    build :: ListT.ListT m LibraryFragment -> m ()
  }
