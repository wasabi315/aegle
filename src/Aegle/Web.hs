module Aegle.Web
  ( runServer,
    Config (..),
  )
where

import Aegle.Core.Isomorphism
import Aegle.Core.Name
import Aegle.Core.Term
import Aegle.Database.Backend
import Aegle.Prelude
import Aegle.Search qualified as Search
import Data.Text qualified as T
import Data.Time.Clock
import Lucid hiding (for_)
import Lucid.Servant
import Network.URI.Encode qualified
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger
import Paths_aegle
import Prettyprinter
import Servant
import Servant.HTML.Lucid
import System.FilePath

--------------------------------------------------------------------------------

data Config = Config
  { port :: Warp.Port,
    dbReader :: DbReader IO,
    agdaHtmlDir :: FilePath
  }

runServer :: Config -> IO ()
runServer Config {..} = do
  dataDir <- getDataDir
  let staticDir = dataDir </> "static"
  putStrLn $ "Listening on port " ++ show port
  Warp.run port $ middleware $ serve api (server staticDir agdaHtmlDir dbReader)

middleware :: Middleware
middleware = logStdout

--------------------------------------------------------------------------------
-- APIs

type API =
  SearchAPI
    :<|> SearchUI
    :<|> "ping" :> Get '[PlainText] T.Text
    :<|> "static" :> Raw -- static assets like css and favicon
    :<|> "agda" :> Raw -- Agda HTML

type SearchAPI =
  "search"
    :> QueryParam' '[Required, Strict] "q" T.Text
    :> Get '[JSON] Result

type SearchUI =
  QueryParam "q" T.Text
    :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: FilePath -> FilePath -> DbReader IO -> Server API
server staticDir agdaHtmlDir dbReader =
  search dbReader
    :<|> searchUI dbReader
    :<|> pure "pong"
    :<|> serveDirectoryFileServer staticDir
    :<|> serveDirectoryFileServer agdaHtmlDir

--------------------------------------------------------------------------------
-- JSON API

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
    solution :: T.Text,
    moduleName :: ModuleName,
    position :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

search :: DbReader IO -> Server SearchAPI
search dbReader = \query -> do
  guard (not $ T.null $ T.strip query)
    ??: err400 {errBody = "Query parameter q must not be empty"}
  result <-
    liftIO (Search.search config query)
      ?% convertError
  pure $! convertResult result
  where
    config =
      Search.Config
        { querySrc = "<query param>",
          timeout = 3000000,
          ..
        }

    convertResult Search.Result {..} = do
      let sorted = sortOn (termSize . (.solution)) matches
      Result {matches = map convertMatch sorted, ..}

    convertMatch Search.Match {item = LibraryItem {..}, ..} =
      Match
        { signature = T.show $ pretty $ Unqualified signature,
          iso = T.show $ pretty iso,
          solution = T.show $ pretty $ Unqualified solution,
          ..
        }

    convertError e = case e of
      Search.ParseError {} -> err400 {errBody = fromString $ displayException e}
      Search.NotFound {} -> err404 {errBody = fromString $ displayException e}
      Search.Timeout -> err422 {errBody = fromString $ displayException e}

--------------------------------------------------------------------------------
-- Web interface

searchUI :: DbReader IO -> Server SearchUI
searchUI dbReader = \query -> do
  let query' = filter (not . T.null . T.strip) query
  result <- for query' $ liftIO . Search.search config

  pure $ layoutHtml do
    h1_ do
      a_ [hrefTop Nothing] "Aegle 🦅"

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
    config =
      Search.Config
        { querySrc = "<query param>",
          timeout = 3000000,
          ..
        }

    hrefTop = safeAbsHref_ @SearchUI api Proxy

    -- Ref: Agda.Interaction.Highlighting.HTML.Base.annotate
    hrefDefSite modName pos =
      href_
        $ T.pack
        $ concat
          [ "/agda/",
            Network.URI.Encode.encode $ T.unpack (coerce modName),
            ".html#",
            Network.URI.Encode.encode $ show pos
          ]

    introHtml :: Html ()
    introHtml = do
      p_ [class_ "intro"] "Type-based library search for Agda"
      section_ do
        h2_ "Example queries"
        ul_ do
          li_ do
            exampleHtml
              "Search by type"
              "(m n : Nat) → _≡_ Nat (_*_ m n) 1 → _≡_ Nat m 1"
          li_ do
            exampleHtml
              "Isomorphism and instantiation"
              "(A B : Set) → (A → B) → A → B"
          li_ do
            exampleHtml
              "Type alias expansion"
              "Commutative Nat (_≡_ Nat) _+_"
      where
        exampleHtml label query = do
          toHtml @T.Text label
          ": "
          a_ [hrefTop (Just query)] do
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
        _ -> ul_ $ for_ sorted \Search.Match {item = LibraryItem {..}, ..} -> li_ [class_ "match"] do
          code_ [class_ "match-heading"] do
            a_ [hrefDefSite moduleName position] do
              strong_ $ prettyHtml canonicalName
            " : "
            prettyHtml $ Unqualified signature
          div_ [class_ "match-details"] do
            unless (null reexportedAs) do
              p_ [class_ "detail-row"] do
                strong_ "Re-exported as: "
                sequence_ $ intersperse ", " do
                  code_ . prettyHtml <$> reexportedAs
            case (iso, solution) of
              (Refl, Top {}) -> pure ()
              (Refl, _) -> details_ do
                summary_ "Solution"
                p_ [class_ "detail-row"] do
                  code_ $ prettyHtml $ Unqualified solution
              _ -> details_ do
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
    title_ "Aegle 🦅"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/style.css"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  body_ content

prettyHtml :: (Pretty a, Monad m) => a -> HtmlT m ()
prettyHtml = toHtml . T.show . pretty
