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
import System.Exit

--------------------------------------------------------------------------------
-- Options

data Command = Command
  { connSetting :: Setting,
    libraryDir :: FilePath,
    transparentDefsFile :: FilePath
  }

index :: Command -> IO ()
index Command {..} = do
  transparentDefNames <- orDie $ eitherDecodeFileStrict transparentDefsFile
  withConnect connSetting \conn -> do
    migrate conn
    let dbBuilder = newDbBuilder conn
    Index.indexLibrary Index.Config {..} dbBuilder

--------------------------------------------------------------------------------

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

withConnect :: Setting -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connSetting]) release
