module TypeSearch.Web
  ( runServer,
    Config (..),
  )
where

import Data.Text qualified as T
import Data.Time.Clock
import Network.Wai.Handler.Warp qualified as Warp
import Prettyprinter
import Servant
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Database.Backend
import TypeSearch.Prelude
import TypeSearch.Search qualified as Search

--------------------------------------------------------------------------------

data Config = Config
  { port :: Warp.Port,
    dbReader :: DbReader IO
  }

runServer :: Config -> IO ()
runServer Config {..} = do
  putStrLn $ "Listening on port " ++ show port
  Warp.run port $ serve api (server dbReader)

--------------------------------------------------------------------------------
-- APIs

type API = PingAPI :<|> SearchAPI

type PingAPI = "ping" :> Get '[PlainText] T.Text

type SearchAPI = "search" :> QueryParam "type" T.Text :> Get '[JSON] Result

api :: Proxy API
api = Proxy

server :: DbReader IO -> Server API
server dbReader = ping :<|> search dbReader

ping :: Server PingAPI
ping = pure "pong"

--------------------------------------------------------------------------------
-- Search

data Result = Result
  { numCands :: Int,
    time :: NominalDiffTime,
    matches :: [Match]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Match = Match
  { canonicalName :: QName,
    reexportedAs :: [QName],
    -- return prettyprinted terms for now
    signature :: T.Text,
    iso :: T.Text,
    solution :: T.Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

search :: DbReader IO -> Server SearchAPI
search dbReader = \case
  Nothing -> pure Result {numCands = 0, time = 0, matches = []}
  Just query -> do
    result <- liftIO $ Search.search dbReader query
    case result of
      Right result -> pure $! convert result
      Left (Search.ParseError e) ->
        throwError
          err400
            { errBody = fromString $ displayException e
            }
      Left (Search.NotFound n) ->
        throwError
          err404
            { errBody = "not found: " <> fromString (show $ pretty n)
            }
  where
    convert Search.Result {..} =
      Result
        { matches =
            matches <&> \Search.Match {item = LibraryItem {..}, ..} ->
              Match
                { signature = T.show $ pretty $ Unqualified signature,
                  iso = T.show $ pretty iso,
                  solution = T.show $ pretty $ Unqualified solution,
                  ..
                },
          ..
        }
