module Aegle.Search
  ( search,
    Config (..),
    Result (..),
    Match (..),
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
        feats = nubOrd $ mapMaybe (allFeatureQ . fst) typs
        compats = feats <&> \feat -> toCompat ! #query feat

    -- 4. Load candidates
    cands <- liftIO $ loadCandidates config.dbReader names compats

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
onTimeout s x m = fromMaybe x <$> timeout s m
