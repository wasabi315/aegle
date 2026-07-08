module Aegle.Search
  ( search,
    Config (..),
    Result (..),
    Match (..),
    CandTime (..),
    Error (..),
  )
where

import Aegle.Core.Evaluation
import Aegle.Core.Isomorphism
import Aegle.Core.Name
import Aegle.Core.Term
import Aegle.Database.Backend
import Aegle.Prelude
import Aegle.Search.Feature
import Aegle.Search.Match
import Aegle.Search.Parser
import Aegle.Search.Query qualified as Q
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

--------------------------------------------------------------------------------
-- Types

data Config = Config
  { dbReader :: DbReader IO,
    querySrc :: String,
    timeout :: Int,
    recordCandTimes :: Bool
  }

data Result = Result
  { numCands :: Int,
    matches :: [Match],
    time :: NominalDiffTime,
    candTimes :: Maybe [CandTime]
  }
  deriving stock (Show, Generic)

data Match = Match
  { item :: {-# UNPACK #-} LibraryItem,
    iso :: Iso,
    solution :: Term
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data CandTime = CandTime
  { name :: QName,
    time :: NominalDiffTime,
    matched :: Bool
  }
  deriving stock (Show, Generic)

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
  ((numCands, matches, candTimes), time) <- timed do
    -- 1. parse query
    Q.Query {..} <- parseQuery config.querySrc query ??% ParseError

    -- 2. resolve free variables and obtain 'Resol' and 'TopEnv'
    refMap <- liftIO $ resolveNames config.dbReader $ M.fromSet id (Q.freeVars typ)
    resol <- flip M.traverseWithKey refMap \x refs ->
      fmap (S1.fromList . fmap (.canonicalName)) (NE.nonEmpty refs)
        ??: NotFound x
    let mctx = emptyMetaCtx resol
        tenv = flip foldMap (Compose refMap) \Referent {..} ->
          foldMap
            (ML.singleton canonicalName . eval mempty mctx [])
            body

    -- 3. Speculatively normalise the query type and compute possible features
    let typ' = Q.toTerm typ
        typs = quoteNondet mctx 0 (eval tenv mctx [] typ')
        feats = nubOrd $ mapMaybe (filterFeatureQ . fst) typs
        compats = feats <&> \feat -> toCompat ! #query feat

    -- 4. Load candidates
    cands <- liftIO $ loadCandidates config.dbReader names compats
    let numCands = length cands

    -- 4. Try matching
    if config.recordCandTimes
      then liftIO do
        (matches, candTimes) <- matchWithTime tenv resol typ' cands
        pure (numCands, matches, Just candTimes)
      else liftIO do
        matches <- match tenv resol typ' cands
        pure (numCands, matches, Nothing)

  pure Result {..}

match :: TopEnv -> Resol -> Type -> [LibraryItem] -> IO [Match]
match tenv resol query items =
  Streamly.fromList items
    & Streamly.parConcatMap
      id
      ( \item@LibraryItem {..} ->
          case IStr.streamToMaybe $ check0 tenv resol query canonicalName signature of
            Nothing -> Streamly.nil
            Just (iso, solution) -> Streamly.fromPure $! Match {..}
      )
    & Streamly.toList

matchWithTime :: TopEnv -> Resol -> Type -> [LibraryItem] -> IO ([Match], [CandTime])
matchWithTime tenv resol query items = do
  results <- for items \item@LibraryItem {..} ->
    (item,) <$> timedPure do
      IStr.streamToMaybe $ check0 tenv resol query canonicalName signature
  let candTimes =
        results <&> \(item, (result, time)) ->
          CandTime {name = item.canonicalName, matched = isJust result, ..}
      matches =
        flip mapMaybe results \(item, (result, _)) -> do
          (iso, solution) <- result
          pure Match {..}
  pure (matches, candTimes)

--------------------------------------------------------------------------------

onTimeout :: Int -> a -> IO a -> IO a
onTimeout s x m = fromMaybe x <$> timeout s m
