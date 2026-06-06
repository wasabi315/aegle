module TypeSearch.Cli.Index
  ( index,
    Command (..),
  )
where

import Control.Exception
import Data.Aeson (eitherDecodeFileStrict)
import Hasql.Connection
import Hasql.Connection.Setting
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Options.Applicative
import System.Exit
import System.FilePath
import TypeSearch.Database.Backend.PostgreSQL
import TypeSearch.Index qualified as Index
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Options

data Command = Command
  { connSetting :: ConnSetting.Connection,
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

withConnect :: ConnSetting.Connection -> (Connection -> IO r) -> IO r
withConnect connInfo =
  bracket (orDie $ first show <$> acquire [connection connInfo]) release
