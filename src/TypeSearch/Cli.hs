module TypeSearch.Cli (main) where

import Control.Exception
import Data.Aeson (eitherDecodeFileStrict)
import Data.Maybe
import Data.Text qualified as T
import Hasql.Connection
import Hasql.Connection.Setting
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Hasql.Connection.Setting.Connection.Param
import Options.Applicative
import System.Environment (getEnv, lookupEnv)
import System.Exit
import System.FilePath
import TypeSearch.Database.Backend.PostgreSQL
import TypeSearch.Database.Index qualified as Index
import TypeSearch.Database.Search qualified as Search
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Options

getConnectInfo :: IO ConnSetting.Connection
getConnectInfo = do
  host <- host . T.pack . fromMaybe "127.0.0.1" <$> lookupEnv "DATABASE_HOST"
  port <- port . maybe 5432 read <$> lookupEnv "DATABASE_PORT"
  user <- user . T.pack <$> getEnv "DATABASE_USER"
  password <- password . T.pack <$> getEnv "DATABASE_PASSWORD"
  database <- dbname . T.pack <$> getEnv "DATABASE_NAME"
  pure $ ConnSetting.params [host, port, user, password, database]

data Command
  = Index IndexCommand
  | Search SearchCommand
  | InteractiveSearch InteractiveSearchCommand

data IndexCommand = IndexCommand
  { libraryDir :: FilePath,
    transparentDefsFile :: FilePath
  }

newtype SearchCommand = SearchCommand
  { query :: T.Text
  }

data InteractiveSearchCommand = InteractiveSearchCommand

optIndexCommand :: Parser IndexCommand
optIndexCommand =
  IndexCommand
    <$> strArgument (metavar "LIBRARY_DIR")
    <*> strArgument (metavar "TRANSPARENT_DEFS_FILE")

optSearchCommand :: Parser SearchCommand
optSearchCommand =
  SearchCommand
    <$> strArgument (metavar "TRANSPARENT_DEFS_FILE")

optInteractiveSearchCommand :: Parser InteractiveSearchCommand
optInteractiveSearchCommand =
  pure InteractiveSearchCommand

commandDesc :: String -> String -> Parser a -> Mod CommandFields a
commandDesc cmd desc p = command cmd $ info p (progDesc desc)

optCommand :: Parser Command
optCommand =
  hsubparser
    $ mconcat
      [ commandDesc "index" "Index an Agda Library" do
          Index <$> optIndexCommand,
        commandDesc "search" "Search within indexed library" do
          Search <$> optSearchCommand,
        commandDesc "interactive" "Interactive search shell" do
          InteractiveSearch <$> optInteractiveSearchCommand
      ]

main :: IO ()
main = do
  command <- execParser (info (optCommand <**> helper) fullDesc)
  connInfo <- getConnectInfo
  dispatchCommand command connInfo

--------------------------------------------------------------------------------

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

dispatchCommand :: Command -> ConnSetting.Connection -> IO ()
dispatchCommand = \case
  Index cmd -> index cmd
  Search cmd -> search cmd
  InteractiveSearch cmd -> interactive cmd

withConnect :: ConnSetting.Connection -> (Connection -> IO r) -> IO r
withConnect connInfo =
  bracket (orDie $ first show <$> acquire [connection connInfo]) release

index :: IndexCommand -> ConnSetting.Connection -> IO ()
index IndexCommand {..} connInfo = do
  transparentDefNames <- orDie $ eitherDecodeFileStrict transparentDefsFile
  withConnect connInfo \conn -> do
    migrate conn
    let dbBuilder = newDbBuilder conn
    Index.indexLibrary Index.Config {..} dbBuilder

search :: SearchCommand -> ConnSetting.Connection -> IO ()
search SearchCommand {..} connInfo = do
  withConnect connInfo \conn -> do
    let dbReader = newDbReader conn
    Search.search dbReader query

interactive :: InteractiveSearchCommand -> ConnSetting.Connection -> IO ()
interactive InteractiveSearchCommand connInfo = do
  withConnect connInfo \conn -> do
    let dbReader = newDbReader conn
    Search.interactive dbReader
