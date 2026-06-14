module Aegle.Cli.Index
  ( index,
    Command (..),
  )
where

import Aegle.Database.Backend.PostgreSQL
import Aegle.Index qualified as Index
import Aegle.Prelude
import Control.Exception
import Data.Aeson (eitherDecodeFileStrict)
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
  { libs :: [RawLibraryConfig]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data RawLibraryConfig = RawLibraryConfig
  { dir :: FilePath,
    transparentDefs :: Maybe FilePath
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

loadConfigFile :: FilePath -> IO Index.Config
loadConfigFile configFile = do
  configFile' <- makeAbsolute configFile
  let configDir = takeDirectory configFile'
  rawConfig <- orDie $ eitherDecodeFileStrict @RawConfig configFile'
  libraryConfigs <- traverse (loadLibraryConfig configDir) rawConfig.libs
  pure Index.Config {..}

loadLibraryConfig :: FilePath -> RawLibraryConfig -> IO Index.LibraryConfig
loadLibraryConfig absConfigDir RawLibraryConfig {..} = do
  let libraryDir = resolvePath absConfigDir dir
  transparentDefNames <-
    fromMaybe mempty <$> for transparentDefs \path -> orDie do
      let absPath = resolvePath absConfigDir path
      eitherDecodeFileStrict absPath
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
