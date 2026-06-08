module TypeSearch.Web
  ( runServer,
    Config (..),
  )
where

import Data.Text qualified as T
import Data.Time.Clock
import Lucid hiding (for_)
import Lucid.Servant
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
    result <- liftIO $ Search.search dbReader "<query param>" query
    case result of
      Right result -> pure $! convert result
      Left e@Search.ParseError {} ->
        throwError
          err400
            { errBody = fromString $ displayException e
            }
      Left e@Search.NotFound {} ->
        throwError
          err404
            { errBody = fromString $ displayException e
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
  result <- for query $ liftIO . Search.search dbReader "<query param>"

  pure $ layoutHtml do
    h1_ do
      a_ [link Nothing] "Dependent type search"

    form_ [method_ "get", action_ "/"] do
      input_
        [ type_ "search",
          name_ "q",
          placeholder_ "Query type",
          value_ $ fromMaybe "" query,
          autofocus_
        ]
      button_ [type_ "submit"] "Search"

    case result of
      Nothing -> introHtml
      Just (Right result) -> resultHtml result
      Just (Left e) ->
        pre_ $ code_ [class_ "error"] $ toHtml $ displayException e
  where
    link = safeAbsHref_ api (Proxy @WebUI)

    introHtml :: Html ()
    introHtml = do
      p_ [class_ "intro"] "Type-based library search for Agda"
      section_ do
        h2_ "Example queries"
        ul_ do
          li_ do
            exampleHtml
              "Search by type"
              "(m n : Nat) -> _≡_ Nat (_*_ m n) 1 -> _≡_ Nat m 1"
          li_ do
            exampleHtml
              "Isomorphism and instantiation"
              "(A B : U) → B → (B × A → B) → List A → B"
          li_ do
            exampleHtml
              "Type alias expansion"
              "Commutative Nat (_≡_ Nat) _+_"
      where
        exampleHtml label query = do
          toHtml @T.Text label
          ": "
          a_ [link (Just query)] do
            code_ [class_ "example-query"] do
              toHtml @T.Text query

    resultHtml :: Search.Result -> Html ()
    resultHtml Search.Result {..} = do
      let numMatches = length matches
          sorted = sortOn (termSize . (.solution)) matches

      p_ [class_ "meta"] do
        toHtml $ T.show numMatches
        " item(s) matched in "
        toHtml $ T.show numCands
        " candidate(s). Took "
        toHtml $ T.show time
        "."
      case matches of
        [] -> p_ "No matches."
        _ -> ul_ $ for_ sorted \Search.Match {item = LibraryItem {..}, ..} -> li_ do
          code_ [class_ "match-heading"] do
            strong_ $ prettyHtml canonicalName
            " : "
            prettyHtml $ Unqualified signature
          div_ [class_ "match-details"] do
            case reexportedAs of
              [] -> pure ()
              _ -> p_ [class_ "detail-row"] do
                strong_ "Re-exported as: "
                sequence_ $ intersperse ", " do
                  code_ . prettyHtml <$> reexportedAs
            details_ do
              summary_ "Isomorphism and solution"
              p_ [class_ "detail-row"] do
                strong_ "Isomorphism: "
                code_ $ prettyHtml iso
              p_ [class_ "detail-row"] do
                strong_ "Solution: "
                code_ $ prettyHtml $ Unqualified solution

layoutHtml :: Html () -> Html ()
layoutHtml content = doctypehtml_ do
  head_ do
    title_ "Dependent type search"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/style.css"]
  body_ content

prettyHtml :: (Pretty a, Monad m) => a -> HtmlT m ()
prettyHtml = toHtml . T.show . pretty
