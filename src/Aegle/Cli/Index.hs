{-# LANGUAGE ApplicativeDo #-}

module Aegle.Cli.Index
  ( index,
    Command (..),
  )
where

import Aegle.Database.Backend.PostgreSQL
import Aegle.Index qualified as Index
import Aegle.Index.Statistics
import Aegle.Prelude
import Aegle.Search.Feature
import Control.Exception
import Control.Foldl qualified as Foldl
import Data.Map.Strict qualified as M
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Yaml
import Deriving.Aeson
import Hasql.Connection
import Hasql.Connection.Setting
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Directory
import System.Exit
import System.FilePath

--------------------------------------------------------------------------------
-- Options

data Command = Command
  { connSetting :: Setting,
    configFile :: FilePath,
    enableStatistics :: Bool
  }

--------------------------------------------------------------------------------

index :: Command -> IO ()
index Command {..} = do
  config <- loadConfigFile configFile
  cwd <- getCurrentDirectory
  createDirectoryIfMissing True (cwd </> ".aegle-work")
  withConnect connSetting \conn -> do
    migrate conn
    let builder = do
          newDbBuilder conn
          when enableStatistics do
            Foldl.postmapM putStatistics $ Foldl.generalize statisticsBuilder
          pure ()
        logger (arg #module -> mod) (arg #log -> log) =
          T.appendFile (cwd </> ".aegle-work" </> T.unpack mod <.> "log") log
    Index.index config logger builder

--------------------------------------------------------------------------------
-- Load config

newtype RawConfig = RawConfig
  { libraries :: [RawLibraryConfig]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data RawLibraryConfig = RawLibraryConfig
  { path :: FilePath,
    transparentDefs :: TransparentDefMode,
    exclusions :: Maybe (S.Set T.Text)
  }
  deriving stock (Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] RawLibraryConfig

data TransparentDefMode
  = None
  | Auto
  deriving stock (Generic)
  deriving
    (FromJSON)
    via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] TransparentDefMode

loadConfigFile :: FilePath -> IO Index.Config
loadConfigFile configFile = do
  configFile' <- makeAbsolute configFile
  let configDir = takeDirectory configFile'
  rawConfig <- decodeFileThrow @_ @RawConfig configFile'
  let libraryConfigs =
        rawConfig.libraries <&> \config -> do
          let path = resolvePath configDir config.path
              transparentDefPolicy = case (config.transparentDefs, config.exclusions) of
                (None, _) -> Index.None
                (Auto, exc) -> Index.AllExcept $ fold exc
          Index.LibraryConfig {..}
  pure Index.Config {..}

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath base path
  | isAbsolute path = normalise path
  | otherwise = normalise $ base </> path

--------------------------------------------------------------------------------
-- Statistics

putStatistics :: Statistics -> IO ()
putStatistics Statistics {..} = putDoc statsDoc
  where
    statsDoc =
      vsep
        [ "Statistics",
          numItemDoc,
          numItemPerFeatureShapeDoc,
          numItemPerArityDoc,
          numItemPerRHTopDoc,
          numCanonicalNamePerUnqualNameDoc,
          emptyDoc
        ]

    numItemDoc = "Total items" <+> colon <+> pretty numItem

    numItemPerFeatureShapeDoc =
      "Total items per feature shape"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ featureDoc feat <+> "→" <+> pretty num
                | (feat, num) <- M.toList numItemPerFeatureShape
                ]
          )

    numItemPerArityDoc =
      "Total items per arity"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ arityDoc arity <+> "→" <+> pretty num
                | (arity, num) <- M.toList numItemPerArity
                ]
          )

    numItemPerRHTopDoc =
      "Total items per result head top name"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ pretty name <+> "→" <+> pretty num
                | (name, num) <- sortOn (Down . snd) $ M.toList numItemPerRHTop
                ]
          )

    numCanonicalNamePerUnqualNameDoc =
      "Total canonical names per colliding unqualified name"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ pretty name <+> "→" <+> pretty num
                | (name, num) <- sortOn (Down . snd) $ M.toList numCanonicalNamePerUnqualName,
                  num > 1
                ]
          )

    featureDoc FeatureShape {..} =
      tupled
        [ case resultHead of
            RHU -> "U"
            RHVar -> "Var"
            RHTop {} -> "Top"
            RHSigma -> "Σ"
            RHProj1 -> ".1"
            RHProj2 -> ".2",
          case polymorphic of
            Monomorphic -> "Mono"
            Polymorphic -> "Poly",
          if arityHasVar then "≧" else "="
        ]

    arityDoc Arity {..}
      | hasVar = "≧" <> pretty arity
      | otherwise = "=" <> pretty arity

--------------------------------------------------------------------------------

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

withConnect :: Setting -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connSetting]) release
