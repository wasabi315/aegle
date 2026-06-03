module TypeSearch.Database.Search
  ( interactive,
    search,
  )
where

import Data.ImmatureStream qualified as ImS
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set.NonEmpty qualified as S1
import Data.Text qualified as T
import Data.Time.Clock
import Formatting
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
import TypeSearch.Pretty
import TypeSearch.Unification

--------------------------------------------------------------------------------

search :: DbReader IO -> T.Text -> IO ()
search dbReader typ =
  either putStrLn pure =<< runExceptT do
    ((cands, result), time) <- timed do
      -- 1. parse query type
      typ <- parseQuery "interactive" typ ??% displayException

      -- 2. resolve free variables and obtain resolution table + top env
      let names = Q.freeVars typ
      refMap <- liftIO $ resolveNames dbReader $ M.fromSet id names
      resol <- flip M.traverseWithKey refMap \x refs ->
        fmap (S1.fromList . fmap (.canonicalName)) (NE.nonEmpty refs)
          ??: ("Not found: " ++ prettyPQName x "")
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
      result <- liftIO $ match tenv resol typ' cands
      pure (cands, result)

    -- 5. Show result
    let sorted = sortOn (termSize . (.solution)) result
    liftIO $ displaySearchResults cands sorted time

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
          . ImS.streamToMaybe
          . match' tenv resol query
      )
    & Streamly.toList

match' :: TopEnv -> M.Map PQName (S1.NESet QName) -> Term -> LibraryItem -> ImS.Stream Match
match' tenv resol query item@LibraryItem {..} = do
  let mctx = emptyMetaCtx resol
  (iso, solution) <- check canonicalName (initCtx tenv) resol (eval mctx tenv [] query) (eval mctx tenv [] signature)
  pure $ Match {..}

displaySearchResults :: [a] -> [Match] -> NominalDiffTime -> IO ()
displaySearchResults cands matches time = do
  fprintLn
    (int % " item(s) matched in " % int % " candidate(s)")
    (length matches)
    (length cands)
  fprintLn ("Took " % build % "\n") time

  -- TODO: migrate to formatting
  for_ matches \Match {item = LibraryItem {canonicalName = QName {..}, ..}, ..} -> do
    putStrLn
      $ unlines
      $ concat
        [ [ showString "- " $ prettyName name $ showString " : " $ prettyTerm0 Unqualify signature "",
            showString "  - module         : " $ prettyModuleName moduleName ""
          ],
          case NE.nonEmpty reexportedAs of
            Nothing -> []
            Just re -> [showString "  - re-exported as : " $ fold $ NE.intersperse ", " $ fmap (`prettyQName` "") re],
          case iso of
            Refl -> []
            i -> [showString "  - isomorphism    : " $ prettyIso 0 i ""],
          case solution of
            Top {} -> []
            _ ->
              [ showString "  - solution       : " $ prettyTerm0 Unqualify solution ""
              ]
        ]
