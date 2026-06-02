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
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Names

-- | De Bruijn indices
newtype Index = Index Int
  deriving stock (Generic)
  deriving newtype (Num, Eq, Ord, Show, Hashable, Enum, Flat)

-- | De Bruijn levels
newtype Level = Level Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Num, Show, Hashable, Enum, Flat)

-- | Metavariables
newtype MetaVar = MetaVar Int
  deriving stock (Generic)
  deriving newtype (Num, Eq, Ord, Show, Hashable, Enum, Flat)

-- | Names
newtype Name = Name T.Text
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Hashable, IsString, Flat, ToJSON, FromJSON)

-- | Module names
newtype ModuleName = ModuleName T.Text
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Hashable, IsString, Flat, ToJSON, FromJSON)

-- | Qualified names
data QName = QName
  { moduleName :: ModuleName,
    name :: Name
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Flat, ToJSON, FromJSON)

-- | Possibly-qualified names
data PQName
  = Unqual Name
  | Qual ModuleName Name
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Flat)

instance IsString PQName where
  fromString = Unqual . fromString
