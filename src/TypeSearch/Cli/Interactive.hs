module TypeSearch.Cli.Interactive
  ( interactive,
    Command (..),
  )
where

import Data.Text qualified as T
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Options.Applicative
import System.Console.Repline hiding (Command)
import System.IO
import TypeSearch.Cli.Search qualified as Search
import TypeSearch.Prelude

--------------------------------------------------------------------------------

newtype Command = Command
  { connSetting :: ConnSetting.Connection
  }

--------------------------------------------------------------------------------

-- Interactive search shell
interactive :: Command -> IO ()
interactive Command {..} = evalReplOpts ReplOpts {..}
  where
    banner _ = pure ">> "
    command query =
      liftIO $ Search.search Search.Command {query = T.pack query, ..}
    prefix = Just ':'
    multilineCommand = Nothing
    tabComplete = Word0 (listWordCompleter [])
    initialiser = liftIO $ putStrLn "Welcome to dependent-type-search!"
    finaliser = liftIO $ Exit <$ putStrLn "Bye!"

    options = [("help", help)]
    help _ =
      liftIO
        $ putStrLn
          """
          Commands:
            :help          : show this help text
            <type>         : search for definitions by type

          """
