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
import Data.Yaml
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
    configFile :: FilePath
  }

--------------------------------------------------------------------------------

index :: Command -> IO ()
index Command {..} = do
  config <- loadConfigFile configFile
  withConnect connSetting \conn -> do
    migrate conn
    let builder =
          Foldl.hoists liftIO (newDbBuilder conn)
            *> Foldl.generalize statisticsBuilder
    stats <- Index.index config builder
    putStatistics stats

--------------------------------------------------------------------------------
-- Load config

newtype RawConfig = RawConfig
  { libraries :: [RawLibraryConfig]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data RawLibraryConfig = RawLibraryConfig
  { path :: FilePath,
    transparentDefsFile :: Maybe FilePath
  }
  deriving stock (Generic)

instance FromJSON RawLibraryConfig where
  parseJSON = withObject "RawLibraryConfig" \o ->
    RawLibraryConfig
      <$> (o .: "path")
      <*> (o .:? "transparent_defs_file")

loadConfigFile :: FilePath -> IO Index.Config
loadConfigFile configFile = do
  configFile' <- makeAbsolute configFile
  let configDir = takeDirectory configFile'
  rawConfig <- decodeFileThrow @_ @RawConfig configFile'
  libraryConfigs <- traverse (loadLibraryConfig configDir) rawConfig.libraries
  pure Index.Config {..}

loadLibraryConfig :: FilePath -> RawLibraryConfig -> IO Index.LibraryConfig
loadLibraryConfig absConfigDir config = do
  let path = resolvePath absConfigDir config.path
  transparentDefs <-
    fromMaybe mempty <$> for config.transparentDefsFile \path -> do
      let absPath = resolvePath absConfigDir path
      decodeFileThrow absPath
  pure Index.LibraryConfig {..}

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
          numItemPerFeatureDoc,
          numItemPerRHTopDoc,
          emptyDoc
        ]

    numItemDoc = "Total items" <+> colon <+> pretty numItem

    numItemPerFeatureDoc =
      "Total items per feature"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ featureDoc feat <+> "→" <+> pretty num
                | (feat, num) <- M.toList numItemPerFeature
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

--------------------------------------------------------------------------------

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

withConnect :: Setting -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connSetting]) release
