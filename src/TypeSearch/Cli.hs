{-# LANGUAGE ApplicativeDo #-}

module TypeSearch.Cli (main) where

import Data.Text qualified as T
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Hasql.Connection.Setting.Connection.Param
import OptEnvConf hiding (Command)
import Paths_dependent_type_search
import TypeSearch.Cli.Index qualified as Index
import TypeSearch.Cli.Interactive qualified as Interactive
import TypeSearch.Cli.Search qualified as Search
import TypeSearch.Prelude hiding (reader)

--------------------------------------------------------------------------------
-- Options

data Command
  = Index Index.Command
  | Search Search.Command
  | Interactive Interactive.Command

instance HasParser Command where
  settingsParser =
    commands
      [ index,
        search,
        interactive
      ]
    where
      index = command "index" "Index an Agda library" do
        connSetting <- connectSetting
        libraryDir <-
          setting
            [ argument,
              reader str,
              metavar "LIBRARY_DIR",
              help "Agda library directory to index"
            ]
        transparentDefsFile <-
          setting
            [ argument,
              reader str,
              metavar "TRANSPARENT_DEFS_FILE",
              help "JSON file listing definitions to unfold"
            ]
        pure $ Index Index.Command {..}

      search = command "search" "Search within indexed library" do
        connSetting <- connectSetting
        query <-
          setting
            [ argument,
              reader str,
              metavar "QUERY",
              help "Type expression to search for"
            ]
        pure $ Search Search.Command {..}

      interactive = command "interactive" "Interactive search shell" do
        connSetting <- connectSetting
        pure $ Interactive Interactive.Command {..}

      connectSetting = do
        host <-
          (host . T.pack)
            <$> setting
              [ env "DATABASE_HOST",
                metavar "DATABASE_HOST",
                reader str,
                help "PostgreSQL host"
              ]
        port <-
          port
            <$> setting
              [ env "DATABASE_PORT",
                metavar "DATABASE_PORT",
                reader auto,
                help "PostgreSQL port"
              ]
        user <-
          (user . T.pack)
            <$> setting
              [ env "DATABASE_USER",
                metavar "DATABASE_USER",
                reader str,
                help "PostgreSQL user"
              ]
        password <-
          (password . T.pack)
            <$> setting
              [ env "DATABASE_PASSWORD",
                metavar "DATABASE_PASSWORD",
                reader str,
                help "PostgreSQL password"
              ]
        database <-
          (dbname . T.pack)
            <$> setting
              [ env "DATABASE_NAME",
                metavar "DATABASE_NAME",
                reader str,
                help "PostgreSQL database name"
              ]
        pure $ ConnSetting.params [host, port, user, password, database]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- runSettingsParser version "Type-based library search for Agda"
  case cmd of
    Index cmd -> Index.index cmd
    Search cmd -> Search.search cmd
    Interactive cmd -> Interactive.interactive cmd
