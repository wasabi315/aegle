module TypeSearch.Cli.Search
  ( TypeSearch.Cli.Search.search,
    Command (..),
  )
where

import Control.Exception
import Data.Text qualified as T
import Hasql.Connection
import Hasql.Connection.Setting
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Prettyprinter
import Prettyprinter.Render.Terminal
import Prettyprinter.Util
import System.Exit
import System.IO
import TypeSearch.Core.Isomorphism
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Database.Backend
import TypeSearch.Database.Backend.PostgreSQL
import TypeSearch.Prelude
import TypeSearch.Search as Search

--------------------------------------------------------------------------------

data Command = Command
  { connSetting :: ConnSetting.Connection,
    query :: T.Text
  }

--------------------------------------------------------------------------------

search :: Command -> IO ()
search Command {..} = withConnect connSetting \conn -> do
  let dbReader = newDbReader conn
  result <- Search.search dbReader query
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

    matchDoc Match {item = LibraryItem {canonicalName = QName {..}, ..}, ..} =
      vsep
        [ annotate (bold <> color Green) do
            "∙" <+> pretty name <+> colon <+> pretty (Unqualified signature),
          indent 2
            $ vsep
            $ catMaybes
              [ Just $ "◦ module         :" <+> pretty moduleName,
                case reexportedAs of
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

withConnect :: ConnSetting.Connection -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connection connSetting]) release
