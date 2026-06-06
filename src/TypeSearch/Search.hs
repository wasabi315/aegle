module TypeSearch.Search
  ( Result (..),
    Match (..),
    Error (..),
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
import Streamly.Data.Stream.Prelude qualified as Streamly
import TypeSearch.Core.Evaluation
import TypeSearch.Core.Isomorphism
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Database.Backend
import TypeSearch.Prelude
import TypeSearch.Search.Feature
import TypeSearch.Search.Parser
import TypeSearch.Search.Query qualified as Q
import TypeSearch.Search.Unification

--------------------------------------------------------------------------------
-- Types

data Result = Result
  { numCands :: Int,
    matches :: [Match],
    time :: NominalDiffTime
  }
  deriving stock (Show, Generic)

data Match = Match
  { item :: {-# UNPACK #-} LibraryItem,
    iso :: Iso,
    solution :: Term
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data Error
  = ParseError ParserError
  | NotFound PQName
  deriving stock (Show)

instance Exception Error where
  displayException = \case
    ParseError e -> displayException e
    NotFound n -> "Not found: " ++ show (pretty n)

--------------------------------------------------------------------------------

search :: DbReader IO -> T.Text -> IO (Either Error Result)
search dbReader typ = runExceptT do
  ((numCands, matches), time) <- timed do
    -- 1. parse query type
    typ <- parseQuery "interactive" typ ??% ParseError

    -- 2. resolve free variables and obtain resolution table + top env
    let names = Q.freeVars typ
    refMap <- liftIO $ resolveNames dbReader $ M.fromSet id names
    resol <- flip M.traverseWithKey refMap \x refs ->
      fmap (S1.fromList . fmap (.canonicalName)) (NE.nonEmpty refs)
        ??: NotFound x
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
    pure (length cands, matches)

  pure Result {..}

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
  pure Match {..}
