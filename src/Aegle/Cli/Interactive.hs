module Aegle.Cli.Interactive
  ( interactive,
    Command (..),
  )
where

import Aegle.Cli.Search qualified as Search
import Aegle.Database.Backend.PostgreSQL
import Aegle.Prelude
import Control.Exception
import Data.Text qualified as T
import Hasql.Connection
import Hasql.Connection.Setting
import System.Console.Repline hiding (Command)
import System.Exit

--------------------------------------------------------------------------------

newtype Command = Command
  { connSetting :: Setting
  }

--------------------------------------------------------------------------------

-- Interactive search shell
interactive :: Command -> IO ()
interactive Command {..} =
  withConnect connSetting \conn -> do
    let dbReader = newDbReader conn
    evalReplOpts ReplOpts {command = command dbReader, ..}
  where
    banner _ = pure ">> "
    command dbReader =
      liftIO . Search.searchWith dbReader . T.pack
    prefix = Just ':'
    multilineCommand = Nothing
    tabComplete = Word0 (listWordCompleter [])
    initialiser = liftIO $ putStrLn "Welcome to Aegle!"
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

--------------------------------------------------------------------------------

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

withConnect :: Setting -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connSetting]) release
