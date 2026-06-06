{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module TypeSearch.Web
  ( runServer,
    Config (..),
  )
where

import Data.Text qualified as T
import Data.Time.Clock
import Lucid hiding (for_)
import Network.Wai.Handler.Warp qualified as Warp
import Paths_dependent_type_search
import Prettyprinter
import Servant
import Servant.HTML.Lucid
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
  staticDir <- getDataFileName "static"
  Warp.run port $ serve api (server staticDir dbReader)

--------------------------------------------------------------------------------
-- APIs

type API =
  PingAPI
    :<|> SearchAPI
    :<|> WebUI
    :<|> "static" :> Raw

type PingAPI = "ping" :> Get '[PlainText] T.Text

type SearchAPI = "search" :> QueryParam "q" T.Text :> Get '[JSON] Result

type WebUI = QueryParam "q" T.Text :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: FilePath -> DbReader IO -> Server API
server staticDir dbReader =
  ping
    :<|> search dbReader
    :<|> webUI dbReader
    :<|> serveDirectoryFileServer staticDir

ping :: Server PingAPI
ping = pure "pong"

--------------------------------------------------------------------------------
-- JSON

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
            sortOn (termSize . (.solution)) matches <&> \Search.Match {item = LibraryItem {..}, ..} ->
              Match
                { signature = T.show $ pretty $ Unqualified signature,
                  iso = T.show $ pretty iso,
                  solution = T.show $ pretty $ Unqualified solution,
                  ..
                },
          ..
        }

--------------------------------------------------------------------------------
-- HTML

webUI :: DbReader IO -> Server WebUI
webUI dbReader query = do
  result <- traverse (liftIO . Search.search dbReader) query

  pure $ doctypehtml_ do
    head_ do
      title_ "Dependent type search"
      link_ [rel_ "stylesheet", href_ "/static/style.css"]

    body_ do
      h1_ "Dependent type search"

      form_ [method_ "get", action_ "/"] do
        input_
          [ type_ "search",
            name_ "type",
            placeholder_ "Query type",
            value_ $ fromMaybe "" query,
            autofocus_
          ]
        button_ [type_ "submit"] "Search"

      case result of
        Nothing ->
          pure ()
        Just (Right result) -> resultHtml result
        Just (Left e) ->
          p_ [class_ "error"] $ toHtml $ displayException e
  where
    resultHtml Search.Result {..} = do
      let numMatches = length matches
          sorted = sortOn (termSize . (.solution)) matches
      p_ [class_ "meta"] do
        toHtml $ T.show numMatches
        " item(s) matched in "
        toHtml $ T.show numCands
        " candidate(s)"
      p_ [class_ "meta"] $ toHtml do
        "Took "
        toHtml $ T.show time
      case matches of
        [] -> p_ "No matches."
        _ -> ul_ do
          for_ sorted \Search.Match {item = LibraryItem {..}, ..} -> do
            li_ do
              strong_ $ toHtml $ T.show $ pretty canonicalName
              span_ " : "
              code_ $ toHtml $ T.show $ pretty $ Unqualified signature
              div_ [class_ "match-details"] do
                case reexportedAs of
                  [] -> pure ()
                  _ -> p_ [class_ "detail-row"] do
                    strong_ "Re-exported as: "
                    toHtml $ T.intercalate ", " $ fmap (T.show . pretty) reexportedAs
                details_ do
                  summary_ "Isomorphism and solution"
                  p_ [class_ "detail-row"] do
                    strong_ "Isomorphism: "
                    code_ $ toHtml $ T.show $ pretty iso
                  p_ [class_ "detail-row"] do
                    strong_ "Solution: "
                    code_ $ toHtml $ T.show $ pretty $ Unqualified solution
