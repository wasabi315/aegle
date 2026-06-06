{-# LANGUAGE ApplicativeDo #-}

module TypeSearch.Cli (main) where

import Data.Maybe
import Data.Text qualified as T
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Hasql.Connection.Setting.Connection.Param
import Options.Applicative
import System.Environment (getEnv, lookupEnv)
import TypeSearch.Cli.Index qualified as Index
import TypeSearch.Cli.Interactive qualified as Interactive
import TypeSearch.Cli.Search qualified as Search
import TypeSearch.Prelude

--------------------------------------------------------------------------------
-- Options

getConnectSetting :: IO ConnSetting.Connection
getConnectSetting = do
  host <- host . T.pack . fromMaybe "127.0.0.1" <$> lookupEnv "DATABASE_HOST"
  port <- port . maybe 5432 read <$> lookupEnv "DATABASE_PORT"
  user <- user . T.pack <$> getEnv "DATABASE_USER"
  password <- password . T.pack <$> getEnv "DATABASE_PASSWORD"
  database <- dbname . T.pack <$> getEnv "DATABASE_NAME"
  pure $ ConnSetting.params [host, port, user, password, database]

data Command
  = Index Index.Command
  | Search Search.Command
  | Interactive Interactive.Command

optIndexCommand :: Parser (ConnSetting.Connection -> Index.Command)
optIndexCommand = do
  libraryDir <- strArgument (metavar "LIBRARY_DIR")
  transparentDefsFile <- strArgument (metavar "TRANSPARENT_DEFS_FILE")
  pure \connSetting -> Index.Command {..}

optSearchCommand :: Parser (ConnSetting.Connection -> Search.Command)
optSearchCommand = do
  query <- strArgument (metavar "QUERY")
  pure \connSetting -> Search.Command {..}

optInteractiveSearchCommand :: Parser (ConnSetting.Connection -> Interactive.Command)
optInteractiveSearchCommand =
  pure Interactive.Command

commandDesc :: String -> String -> Parser a -> Mod CommandFields a
commandDesc cmd desc p = command cmd $ info p (progDesc desc)

optCommand :: Parser (ConnSetting.Connection -> Command)
optCommand =
  hsubparser
    $ mconcat
      [ commandDesc "index" "Index an Agda Library" do
          (Index .) <$> optIndexCommand,
        commandDesc "search" "Search within indexed library" do
          (Search .) <$> optSearchCommand,
        commandDesc "interactive" "Interactive search shell" do
          (Interactive .) <$> optInteractiveSearchCommand
      ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  command <- execParser (info (optCommand <**> helper) fullDesc)
  connSetting <- getConnectSetting
  case command connSetting of
    Index cmd -> Index.index cmd
    Search cmd -> Search.search cmd
    Interactive cmd -> Interactive.interactive cmd
