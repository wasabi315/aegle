module TypeSearch.Search.Match where

import Data.ImmatureStream qualified as IStr
import Data.Map.Strict qualified as M
import Data.Set.NonEmpty qualified as S1
import Prettyprinter
import TypeSearch.Core.Evaluation
import TypeSearch.Core.Isomorphism
import TypeSearch.Core.Name
import TypeSearch.Core.Term hiding (rename)
import TypeSearch.Prelude
import TypeSearch.Search.Unification
import TypeSearch.Search.Unification.ModuloIso

--------------------------------------------------------------------------------
-- Context

data Ctx = Ctx
  { topEnv :: TopEnv,
    level :: Level,
    locals :: Locals
  }

data Locals
  = Here
  | Bind Locals Name ~Term

initCtx :: TopEnv -> Ctx
initCtx topEnv =
  Ctx
    { level = 0,
      locals = Here,
      ..
    }

bind :: MetaCtx -> Ctx -> Name -> VType -> Ctx
bind mctx ctx@Ctx {..} x ~a =
  ctx
    { level = level + 1,
      locals = Bind locals x (quote mctx topEnv level a)
    }

--------------------------------------------------------------------------------

closeTy :: Locals -> Term -> Term
closeTy = \cases
  Here b -> b
  (Bind locs x a) b -> closeTy locs (Pi x a b)

closeTm :: Locals -> Term -> Term
closeTm = \cases
  Here t -> t
  (Bind locs x _) b -> closeTm locs (Lam x b)

check0 :: TopEnv -> Resol -> Term -> QName -> Term -> IStr.Stream (Iso, Term)
check0 tenv resol query itemName item = do
  let ctx = initCtx tenv
      mctx = emptyMetaCtx resol
      query' = eval mctx tenv [] query
      item' = eval mctx tenv [] item
  check mctx ctx query' itemName item'

-- FIXME: currently not considering pi permutation
check :: MetaCtx -> Ctx -> Value -> QName -> Value -> IStr.Stream (Iso, Term)
check mctx ctx query itemName item | traceCheck mctx ctx query itemName item = undefined
check mctx ctx query itemName item =
  asum
    [ do
        (item, inst, mctx) <- possibleInstantiation mctx ctx item (VTop itemName SNil)
        (i, i', mctx) <- IStr.maybeToStream $ listToMaybe $ unifyIso mctx ctx.topEnv ctx.level query item
        guard $ allMetaSolved mctx
        let j = i <> sym i'
            ~sol = closeTm ctx.locals $ quote mctx ctx.topEnv ctx.level $ transportInv j inst
        pure (j, sol),
      IStr.Later do
        (query, mctx) <- choose $ forceAmb' mctx ctx.topEnv query
        case query of
          VPi "_" _ _ -> empty
          VPi x a b -> do
            check mctx (bind mctx ctx x a) (b $ VVar ctx.level) itemName item
          _ -> empty
    ]

-- FIXME: currently not considering pi permutation
possibleInstantiation :: MetaCtx -> Ctx -> Value -> Value -> IStr.Stream (Value, Value, MetaCtx)
possibleInstantiation mctx ctx a ~_ | tracePossibleInstantiation mctx ctx a = undefined
possibleInstantiation mctx ctx a ~inst =
  asum
    [ pure (a, inst, mctx),
      IStr.Later case force mctx ctx.topEnv a of
        VPi "_" _ _ -> empty
        VPi _ a b -> do
          (m, mctx) <- pure $ freshMeta mctx ctx a
          let mv = eval mctx ctx.topEnv (idEnv ctx.level) m
          possibleInstantiation mctx ctx (b mv) (inst $$ mv)
        _ -> empty
    ]

idPruning :: Level -> Pruning
idPruning l = replicate (coerce l) True

freshMeta :: MetaCtx -> Ctx -> Value -> (Term, MetaCtx)
freshMeta mctx ctx a = do
  let ~closed = eval mctx ctx.topEnv [] $ closeTy ctx.locals (quote mctx ctx.topEnv ctx.level a)
      (m, mctx') = newMeta mctx closed
  (AppPruning (Meta m) (idPruning ctx.level), mctx')

--------------------------------------------------------------------------------

traceCheck :: MetaCtx -> Ctx -> Value -> QName -> Value -> Bool
traceCheck mctx ctx query itemName item = traceFalse $ show do
  vsep
    [ "check" <+> pretty itemName,
      "tenv" <+> colon <+> align (pretty ctx.topEnv),
      "mctx" <+> colon <+> align (pretty (ctx.topEnv :⊢ mctx)),
      "ctx size" <+> colon <+> pretty ctx.level,
      "query" <+> colon <+> pretty ((ctx.topEnv, mctx, ctx.level) :⊢ query),
      "item" <+> colon <+> pretty ((ctx.topEnv, mctx, ctx.level) :⊢ item)
    ]

tracePossibleInstantiation :: MetaCtx -> Ctx -> Value -> Bool
tracePossibleInstantiation mctx ctx a = traceFalse $ show do
  vsep
    [ "possibleInstantiation",
      "tenv" <+> colon <+> align (pretty ctx.topEnv),
      "mctx" <+> colon <+> align (pretty (ctx.topEnv :⊢ mctx)),
      "a" <+> colon <+> pretty ((ctx.topEnv, mctx, ctx.level) :⊢ a)
    ]
