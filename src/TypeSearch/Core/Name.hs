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

import Control.Lens.Wrapped
import Data.Text qualified as T
import Flat
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Names

-- | De Bruijn indices
newtype Index = Index Int
  deriving stock (Generic)
  deriving newtype (Num, Eq, Ord, Show, Hashable, Enum, Flat)
  deriving anyclass (Wrapped)

-- | De Bruijn levels
newtype Level = Level Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Num, Show, Hashable, Flat)
  deriving anyclass (Wrapped)

-- | Metavariables
newtype MetaVar = MetaVar Int
  deriving stock (Generic)
  deriving newtype (Num, Eq, Ord, Hashable, Enum, Flat)
  deriving anyclass (Wrapped)

instance Show MetaVar where
  showsPrec _ (MetaVar m) = showString "?M" . shows m

-- | Names
newtype Name = Name T.Text
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Hashable, IsString, Flat, ToJSON, FromJSON)
  deriving anyclass (Wrapped)

instance Show Name where
  showsPrec _ (Name n) = showString (T.unpack n)

-- | Module names
newtype ModuleName = ModuleName T.Text
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Hashable, IsString, Flat, ToJSON, FromJSON)

instance Show ModuleName where
  showsPrec _ (ModuleName n) = showString (T.unpack n)

-- | Qualified names
data QName = QName
  { moduleName :: ModuleName,
    name :: Name
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable, Flat, ToJSON, FromJSON)

instance Show QName where
  showsPrec _ x = shows x.moduleName . showChar '.' . shows x.name

-- | Possibly-qualified names
data PQName
  = Unqual Name
  | Qual ModuleName Name
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsString PQName where
  fromString = Unqual . Name . fromString

instance Show PQName where
  showsPrec _ = \case
    Unqual n -> shows n
    Qual m n -> shows m . showChar '.' . shows n
