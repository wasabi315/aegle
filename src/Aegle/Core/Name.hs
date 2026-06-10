module Aegle.Core.Name
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
import Prettyprinter
import Aegle.Prelude

--------------------------------------------------------------------------------
-- Names

-- | De Bruijn indices
newtype Index = Index Int
  deriving stock (Generic)
  deriving newtype (Num, Eq, Ord, Show, Hashable, Enum, Flat, NFData)

-- | De Bruijn levels
newtype Level = Level Int
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Num, Show, Hashable, Enum, Flat, NFData)

-- | Metavariables
newtype MetaVar = MetaVar Int
  deriving stock (Generic)
  deriving newtype (Num, Eq, Ord, Show, Hashable, Enum, Flat, NFData)

-- | Names
newtype Name = Name T.Text
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Hashable, IsString, Flat, ToJSON, FromJSON, NFData, Pretty)

-- | Module names
newtype ModuleName = ModuleName T.Text
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Hashable, IsString, Flat, ToJSON, FromJSON, NFData, Pretty)

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
-- Prettyprinting

instance Pretty Index where
  pretty (Index i) = "@" <> pretty i

instance Pretty Level where
  pretty (Level l) = "#" <> pretty l

instance Pretty MetaVar where
  pretty (MetaVar m) = "?" <> pretty m

instance Pretty QName where
  pretty QName {..} = pretty @T.Text (coerce moduleName <> "." <> coerce name)

instance Pretty PQName where
  pretty = \case
    Unqual x -> pretty x
    Qual m x -> pretty (QName m x)
