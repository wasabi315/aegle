module TypeSearch.Core.Name
  ( Index (..),
    Level (..),
    MetaVar (..),
    Name (..),
    ModuleName (..),
    QName (..),
    PQName (..),
  )
where

import Data.Text qualified as T
import Flat
import Formatting.Buildable
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Names

-- | De Bruijn indices
newtype Index = Index Int
  deriving stock (Generic)
  deriving newtype (Num, Eq, Ord, Show, Hashable, Enum, Flat, Buildable, NFData)

-- | De Bruijn levels
newtype Level = Level Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Num, Show, Hashable, Enum, Flat, Buildable, NFData)

-- | Metavariables
newtype MetaVar = MetaVar Int
  deriving stock (Generic)
  deriving newtype (Num, Eq, Ord, Show, Hashable, Enum, Flat, Buildable, NFData)

-- | Names
newtype Name = Name T.Text
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Hashable, IsString, Flat, ToJSON, FromJSON, Buildable, NFData)

-- | Module names
newtype ModuleName = ModuleName T.Text
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Hashable, IsString, Flat, ToJSON, FromJSON, Buildable, NFData)

-- | Qualified names
data QName = QName
  { moduleName :: ModuleName,
    name :: Name
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Flat, ToJSON, FromJSON, NFData)

-- | Possibly-qualified names
data PQName
  = Unqual Name
  | Qual ModuleName Name
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Flat, NFData)

instance IsString PQName where
  fromString = Unqual . fromString

--------------------------------------------------------------------------------
-- Format

instance Buildable QName where
  build QName {..} = build @T.Text (coerce moduleName <> "." <> coerce name)

instance Buildable PQName where
  build = \case
    Unqual x -> build x
    Qual m x -> build (QName m x)
