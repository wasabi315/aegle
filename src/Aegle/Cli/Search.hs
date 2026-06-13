module Aegle.Cli.Search
  ( Aegle.Cli.Search.search,
    searchWith,
    Command (..),
  )
where

import Aegle.Core.Isomorphism
import Aegle.Core.Term
import Aegle.Database.Backend
import Aegle.Database.Backend.PostgreSQL
import Aegle.Prelude
import Aegle.Search as Search
import Control.Exception
import Data.Text qualified as T
import Hasql.Connection
import Hasql.Connection.Setting
import Prettyprinter
import Prettyprinter.Render.Terminal
import Prettyprinter.Util
import System.Exit
import System.IO

--------------------------------------------------------------------------------

data Command = Command
  { connSetting :: Setting,
    query :: T.Text
  }

--------------------------------------------------------------------------------

search :: Command -> IO ()
search Command {..} =
  withConnect connSetting \conn -> do
    let dbReader = newDbReader conn
    searchWith dbReader query

searchWith :: DbReader IO -> T.Text -> IO ()
searchWith dbReader query = do
  let config =
        Search.Config
          { querySrc = "<interactive>",
            timeout = 3000000,
            ..
          }
  result <- Search.search config query
  either putError putResult result

putResult :: Result -> IO ()
putResult Result {..} =
  putDoc (doc <> line)
  where
    numMatches = length matches

    doc =
      vsep
        [ numDoc,
          timeDoc,
          case matches of
            [] -> emptyDoc
            _ -> enclose line line matchesDoc
        ]

    numDoc =
      hsep
        [ pretty numMatches,
          plural "item" "items" numMatches,
          reflow "matched in",
          pretty numCands,
          plural "candidate" "candidates" numCands
        ]

    timeDoc = "Took" <+> viaShow time

    matchesDoc =
      concatWith (surround $ line <> line) do
        -- rank by solution size
        matchDoc <$> sortOn (termSize . (.solution)) matches

    matchDoc Match {item = LibraryItem {..}, ..} =
      vsep
        [ annotate (bold <> color Green) do
            "∙" <+> pretty canonicalName <+> colon <+> pretty (Unqualified originalSignature),
          indent 2
            $ vsep
            $ catMaybes
              [ case reexportedAs of
                  [] -> Nothing
                  _ -> Just $ "◦ re-exported as :" <+> hsep (punctuate comma $ pretty <$> reexportedAs),
                case iso of
                  Refl -> Nothing
                  _ -> Just $ "◦ isomorphism    :" <+> pretty iso,
                case solution of
                  Top {} -> Nothing
                  _ -> Just $ "◦ solution       :" <+> pretty (Unqualified solution)
              ]
        ]

putError :: Error -> IO ()
putError = hPutStrLn stderr . displayException

--------------------------------------------------------------------------------

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

withConnect :: Setting -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connSetting]) release
