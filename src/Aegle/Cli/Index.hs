module Aegle.Cli.Index
  ( index,
    Command (..),
  )
where

import Aegle.Database.Backend.PostgreSQL
import Aegle.Index qualified as Index
import Aegle.Prelude
import Control.Exception
import Data.Yaml
import Hasql.Connection
import Hasql.Connection.Setting
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
    let dbBuilder = newDbBuilder conn
    Index.index config dbBuilder

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
  deriving anyclass (FromJSON)

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

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

withConnect :: Setting -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connSetting]) release
