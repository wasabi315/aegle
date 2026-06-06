module TypeSearch.Cli.Serve
  ( serve,
    Command (..),
  )
where

import Control.Exception
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool
import Network.Wai.Handler.Warp qualified as Warp
import System.IO
import TypeSearch.Database.Backend.PostgreSQL
import TypeSearch.Prelude
import TypeSearch.Web

--------------------------------------------------------------------------------

data Command = Command
  { poolConfig :: Pool.Config,
    port :: Warp.Port
  }

--------------------------------------------------------------------------------

serve :: Command -> IO ()
serve Command {..} =
  bracket (Pool.acquire poolConfig) Pool.release \pool -> do
    let dbReader = newDbReader (orThrow . Pool.use pool)
    runServer Config {..}
