module TypeSearch.Database.Search
  ( interactive,
    search,
  )
where

import Data.ImmatureStream qualified as IStr
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set.NonEmpty qualified as S1
import Data.Text qualified as T
import Data.Time.Clock
import Prettyprinter
import Prettyprinter.Render.Terminal
import Prettyprinter.Util
import Streamly.Data.Stream.Prelude qualified as Streamly
import System.Console.Repline
import TypeSearch.Core.Evaluation
import TypeSearch.Core.Isomorphism
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Database.Backend
import TypeSearch.Database.Feature
import TypeSearch.Database.Parser
import TypeSearch.Database.Query qualified as Q
import TypeSearch.Prelude
import TypeSearch.Unification

--------------------------------------------------------------------------------

search :: DbReader IO -> T.Text -> IO ()
search dbReader typ =
  either putStrLn pure =<< runExceptT do
    ((candidates, matches), time) <- timed do
      -- 1. parse query type
      typ <- parseQuery "interactive" typ ??% displayException

      -- 2. resolve free variables and obtain resolution table + top env
      let names = Q.freeVars typ
      refMap <- liftIO $ resolveNames dbReader $ M.fromSet id names
      resol <- flip M.traverseWithKey refMap \x refs ->
        fmap (S1.fromList . fmap (.canonicalName)) (NE.nonEmpty refs)
          ??: ("Not found: " ++ show (pretty x))
      let mctx = emptyMetaCtx resol
          tenv = flip foldMap (Compose refMap) \Referent {..} ->
            maybe
              mempty
              (ML.singleton canonicalName . eval mctx mempty [])
              body

      -- 3. Speculatively normalise the query type and compute possible features
      let typ' = Q.toTerm typ
          typs = filter (not . isLam) $ quoteAmb mctx tenv 0 (eval mctx mempty [] typ')
          feats = nubOrd $ map allFeatureQ typs
          compats = feats <&> \feat -> toCompat ! #query feat

      -- 4. Load candidates based on compats
      cands <- liftIO $ loadByAnyFeature dbReader compats

      -- 4. Try matching
      matches <- liftIO $ match tenv resol typ' cands
      pure (cands, matches)

    -- 5. Show result
    liftIO $ displaySearchResult SearchResult {..}

-- Interactive search shell
interactive :: DbReader IO -> IO ()
interactive dbReader = evalReplOpts ReplOpts {..}
  where
    banner _ = pure ">> "
    command = liftIO . search dbReader . T.pack
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

--------------------------------------------------------------------------------

data SearchResult = SearchResult
  { candidates :: [LibraryItem],
    matches :: [Match],
    time :: NominalDiffTime
  }

data Match = Match
  { item :: {-# UNPACK #-} LibraryItem,
    iso :: Iso,
    solution :: Term
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

match :: TopEnv -> M.Map PQName (S1.NESet QName) -> Type -> [LibraryItem] -> IO [Match]
match tenv resol query items =
  Streamly.fromList items
    & Streamly.parConcatMap
      id
      ( maybe Streamly.nil Streamly.fromPure
          . IStr.streamToMaybe
          . match' tenv resol query
      )
    & Streamly.toList

match' :: TopEnv -> M.Map PQName (S1.NESet QName) -> Term -> LibraryItem -> IStr.Stream Match
match' tenv resol query item@LibraryItem {..} = do
  let mctx = emptyMetaCtx resol
  (iso, solution) <- check canonicalName (initCtx tenv) resol (eval mctx tenv [] query) (eval mctx tenv [] signature)
  pure $ Match {..}

displaySearchResult :: SearchResult -> IO ()
displaySearchResult SearchResult {..} =
  putDoc (doc <> line)
  where
    nCand = length candidates
    nMatch = length matches

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
        [ pretty nMatch,
          plural "item" "items" nMatch,
          reflow "matched in",
          pretty nCand,
          plural "candidate" "candidates" nCand
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
