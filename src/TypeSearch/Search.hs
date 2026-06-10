module TypeSearch.Search
  ( search,
    Config (..),
    Result (..),
    Match (..),
    Error (..),
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
import System.Timeout
import TypeSearch.Core.Evaluation
import TypeSearch.Core.Isomorphism
import TypeSearch.Core.Name
import TypeSearch.Core.Term
import TypeSearch.Database.Backend
import TypeSearch.Prelude
import TypeSearch.Search.Feature
import TypeSearch.Search.Match
import TypeSearch.Search.Parser
import TypeSearch.Search.Query qualified as Q

--------------------------------------------------------------------------------
-- Types

data Config = Config
  { dbReader :: DbReader IO,
    querySrc :: String,
    timeout :: Int
  }

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
  | Timeout
  deriving stock (Show)

instance Exception Error where
  displayException = \case
    ParseError e -> displayException e
    NotFound n -> "Not found: " ++ show (pretty n)
    Timeout -> "Timeout"

--------------------------------------------------------------------------------
-- Entrypoint

search :: Config -> T.Text -> IO (Either Error Result)
search config query = onTimeout config.timeout (Left Timeout) $ runExceptT do
  ((numCands, matches), time) <- timed do
    -- 1. parse query type
    typ <- parseQuery config.querySrc query ??% ParseError

    -- 2. resolve free variables and obtain 'Resol' and 'TopEnv'
    let names = Q.freeVars typ
    refMap <- liftIO $ resolveNames config.dbReader $ M.fromSet id names
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
    cands <- liftIO $ loadByAnyFeature config.dbReader compats

    -- 4. Try matching
    matches <- liftIO $ match tenv resol typ' cands
    pure (length cands, matches)

  pure Result {..}

match :: TopEnv -> Resol -> Type -> [LibraryItem] -> IO [Match]
match tenv resol query items =
  Streamly.fromList items
    & Streamly.parConcatMap
      id
      ( maybe Streamly.nil Streamly.fromPure
          . IStr.streamToMaybe
          . match' tenv resol query
      )
    & Streamly.toList

match' :: TopEnv -> Resol -> Term -> LibraryItem -> IStr.Stream Match
match' tenv resol query item@LibraryItem {..} = do
  (iso, solution) <- check0 tenv resol query canonicalName signature
  pure Match {..}

--------------------------------------------------------------------------------

onTimeout :: Int -> a -> IO a -> IO a
onTimeout s x m = maybe (pure x) pure =<< timeout s m
