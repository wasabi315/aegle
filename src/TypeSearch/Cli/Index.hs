module TypeSearch.Cli.Index
  ( index,
    Command (..),
  )
where

import Control.Exception
import Data.Aeson (eitherDecodeFileStrict)
import Hasql.Connection
import Hasql.Connection.Setting
import System.Exit
import System.FilePath
import TypeSearch.Database.Backend.PostgreSQL
import TypeSearch.Index qualified as Index
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Options

data Command = Command
  { connSetting :: Setting,
    libraryDir :: FilePath,
    transparentDefsFile :: FilePath,
    agdaHtmlDir :: FilePath
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
