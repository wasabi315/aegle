module TypeSearch.Cli.Serve
  ( serve,
    Command (..),
  )
where

import Control.Exception
import Hasql.Connection
import Hasql.Connection.Setting
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit
import System.IO
import TypeSearch.Database.Backend.PostgreSQL
import TypeSearch.Prelude
import TypeSearch.Web

--------------------------------------------------------------------------------

data Command = Command
  { connSetting :: ConnSetting.Connection,
    port :: Warp.Port
  }

--------------------------------------------------------------------------------

serve :: Command -> IO ()
serve Command {..} = withConnect connSetting \conn -> do
  let dbReader = newDbReader conn
  runServer Config {..}

--------------------------------------------------------------------------------

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

withConnect :: ConnSetting.Connection -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connection connSetting]) release
