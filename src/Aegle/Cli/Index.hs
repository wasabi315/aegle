module Aegle.Cli.Index
  ( index,
    Command (..),
  )
where

import Aegle.Database.Backend.PostgreSQL
import Aegle.Index qualified as Index
import Aegle.Index.Statistics
import Aegle.Prelude
import Aegle.Search.Feature
import Control.Exception
import Control.Foldl qualified as Foldl
import Data.Map.Strict qualified as M
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml
import Deriving.Aeson
import Hasql.Connection
import Hasql.Connection.Setting
import Prettyprinter
import Prettyprinter.Render.Terminal
import Prettyprinter.Render.Text
import System.Directory
import System.Exit
import System.FilePath
import System.IO

--------------------------------------------------------------------------------
-- Options

data Command = Command
  { connSetting :: Setting,
    configFile :: FilePath
  }

--------------------------------------------------------------------------------

index :: Command -> IO ()
index Command {..} = do
  config <- loadConfigFile configFile
  logger <- createLogger
  (health, stats) <- withConnect connSetting \conn -> do
    migrate conn
    Index.index config logger do
      liftA2 (,) (newDbBuilder conn) (Foldl.generalize statisticsBuilder)
  logHealth logger health
  logStats logger stats

type Logger = "logName" :? T.Text -> Doc AnsiStyle -> IO ()

createLogger :: IO Logger
createLogger = do
  cwd <- getCurrentDirectory
  let logDir = cwd </> ".aegle-log"
  removePathForcibly logDir
  createDirectory logDir
  pure \(ArgF logName) msg -> case logName of
    Nothing -> Prettyprinter.Render.Terminal.hPutDoc stderr (msg <> line)
    Just logName -> do
      let logFile = logDir </> T.unpack logName <.> "log"
          logDir' = takeDirectory logFile
      createDirectoryIfMissing True logDir'
      withFile logFile AppendMode \hdl ->
        Prettyprinter.Render.Text.hPutDoc hdl (msg <> line)

--------------------------------------------------------------------------------
-- Load config

newtype RawConfig = RawConfig
  { libraries :: [RawLibraryConfig]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data RawLibraryConfig = RawLibraryConfig
  { path :: FilePath,
    transparentDefs :: TransparentDefMode,
    except :: Maybe (S.Set T.Text)
  }
  deriving stock (Generic)
  deriving
    (FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] RawLibraryConfig

data TransparentDefMode
  = None
  | Auto
  deriving stock (Generic)
  deriving
    (FromJSON)
    via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] TransparentDefMode

loadConfigFile :: FilePath -> IO Index.Config
loadConfigFile configFile = do
  configFile' <- makeAbsolute configFile
  let configDir = takeDirectory configFile'
  rawConfig <- decodeFileThrow @_ @RawConfig configFile'
  let libraryConfigs =
        rawConfig.libraries <&> \config -> do
          let path = resolvePath configDir config.path
              transparentDefPolicy = case (config.transparentDefs, config.except) of
                (None, _) -> Index.None
                (Auto, exc) -> Index.AllExcept $ fold exc
          Index.LibraryConfig {..}
  pure Index.Config {..}

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath base path
  | isAbsolute path = normalise path
  | otherwise = normalise $ base </> path

--------------------------------------------------------------------------------
-- Log

logHealth :: Logger -> HealthCheck -> IO ()
logHealth logger HealthCheck {..} = do
  let danglingExportsOk = V.null danglingExports
  unless danglingExportsOk do
    logger ! defaults $ danglingExportsDoc danglingExports

  let healthy = danglingExportsOk
  when healthy do
    logger ! defaults $ annotate (color Green) "No problem found in DB"
  where
    danglingExportsDoc danglingExports =
      vsep
        $ ( annotate (color Yellow) do
              "WARNING: Dangling exports found (Total" <+> pretty (V.length danglingExports) <> ")"
          )
        : [ "・" <+> pretty exportAsQual <+> "→" <+> pretty canonicalName
          | DanglingExport {..} <- V.toList danglingExports
          ]

logStats :: Logger -> Statistics -> IO ()
logStats logger Statistics {..} =
  logger ! paramF #logName (Just "stats") $ statsDoc
  where
    statsDoc =
      vsep
        [ numItemDoc,
          numItemPerFeatureShapeDoc,
          numItemPerArityDoc,
          numItemPerRHTopDoc,
          numCanonicalNamePerUnqualNameDoc,
          emptyDoc
        ]

    numItemDoc = "Total items" <+> colon <+> pretty numItem

    numItemPerFeatureShapeDoc =
      "Total items per feature shape"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ featureDoc feat <+> "→" <+> pretty num
                | (feat, num) <- M.toList numItemPerFeatureShape
                ]
          )

    numItemPerArityDoc =
      "Total items per arity"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ arityDoc arity <+> "→" <+> pretty num
                | (arity, num) <- M.toList numItemPerArity
                ]
          )

    numItemPerRHTopDoc =
      "Total items per result head top name"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ pretty name <+> "→" <+> pretty num
                | (name, num) <- sortOn (Down . snd) $ M.toList numItemPerRHTop
                ]
          )

    numCanonicalNamePerUnqualNameDoc =
      "Total canonical names per colliding unqualified name"
        <+> colon
        <+> nest
          4
          ( vsep
              $ punctuate
                comma
                [ pretty name <+> "→" <+> pretty num
                | (name, num) <- sortOn (Down . snd) $ M.toList numCanonicalNamePerUnqualName,
                  num > 1
                ]
          )

    featureDoc FeatureShape {..} =
      tupled
        [ case resultHead of
            RHU -> "U"
            RHVar -> "Var"
            RHTop {} -> "Top"
            RHSigma -> "Σ"
            RHProj1 -> ".1"
            RHProj2 -> ".2",
          case polymorphic of
            Monomorphic -> "Mono"
            Polymorphic -> "Poly",
          if arityHasVar then "≧" else "="
        ]

    arityDoc Arity {..}
      | hasVar = "≧" <> pretty arity
      | otherwise = "=" <> pretty arity

--------------------------------------------------------------------------------

orDie :: IO (Either String a) -> IO a
orDie m = m >>= either die pure

withConnect :: Setting -> (Connection -> IO r) -> IO r
withConnect connSetting =
  bracket (orDie $ first show <$> acquire [connSetting]) release
