module DAM4G.Compiler.Optimizer.Translate.Monad where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local)
import Control.Monad.State (class MonadState, StateT, get)
import DAM4G.Compiler.Global as G
import DAM4G.Compiler.Name (ConstructorName, GlobalName, Ident, Qualified, TypeName, toIdent)
import DAM4G.Compiler.Optimizer.IR (Path, Var(..))
import DAM4G.Compiler.Syntax.AST as AST
import DAM4G.Compiler.Types (ConstructorTag)
import Data.Array (foldl)
import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Fmt as Fmt
import Partial.Unsafe (unsafeCrashWith)

type TranslState =
  { genv :: G.Env
  }

data TranslEnv
  = TNull
  | TEnv (Map.Map Ident Path) TranslEnv

derive instance Eq TranslEnv
derive instance Generic TranslEnv _
instance Show TranslEnv where
  show it = genericShow it

type TranslContext =
  { locals :: TranslEnv
  }

type TranslM m a = StateT TranslState (ReaderT TranslContext m) a

lookupType :: forall m. MonadState TranslState m => Qualified TypeName -> m G.TypeInfo
lookupType typname = do
  { genv } <- get
  case Map.lookup typname genv.types of
    Nothing -> unsafeCrashWith "Impossible: unknown type"
    Just typeinfo -> pure typeinfo

lookupDecl :: forall m. MonadState TranslState m => GlobalName -> m G.GlobalInfo
lookupDecl gloname = do
  { genv } <- get
  case Map.lookup gloname genv.globals of
    Nothing -> unsafeCrashWith "Impossible: unknown global"
    Just info -> pure info

lookupConstructor :: forall m. MonadState TranslState m => Qualified ConstructorName -> m G.ConstructorDesc
lookupConstructor constrName = do
  lookupDecl (toIdent <$> constrName) >>= _.desc >>> case _ of
    G.Constructor desc -> pure desc
    _ -> unsafeCrashWith "Impossible: Not a constructor"

getConstructorTag :: forall m. MonadState TranslState m => Qualified ConstructorName -> m ConstructorTag
getConstructorTag constrName = do
  constr <- lookupConstructor constrName
  typ <- lookupType constr.typname
  case Array.findIndex (_ == constr.name) typ.constrs of
    Nothing -> unsafeCrashWith $
      Fmt.fmt
        @"Impossible: {constr} is not a constructor of given type {typ}"
        { constr: show constrName
        , typ: show constr.typname
        }
    Just tag -> pure tag

searchIdent
  :: forall m
   . MonadState TranslState m
  => MonadAsk TranslContext m
  => Ident
  -> m (Var /\ Path)
searchIdent ident = do
  { locals } <- ask
  let _ = unsafePerformEffect (logShow locals)
  pure $ go 0 locals
  where
  go :: Int -> TranslEnv -> Var /\ Path
  go n = case _ of
    TNull -> unsafeCrashWith $
      Fmt.fmt @"Impossible: unknown local variable {var}" { var: show ident }
    TEnv vars tenv
      | Just path <- Map.lookup ident vars -> Var n /\ path
      | otherwise -> go (n + 1) tenv

extendWithNewVar :: forall m. MonadReader TranslContext m => Ident -> Path -> m ~> m
extendWithNewVar ident path m = do
  local (\ctx -> ctx { locals = extendEnv ctx.locals }) m
  where
  extendEnv tenv = TEnv (Map.singleton ident path) tenv

extendWithPatterns :: forall m. MonadReader TranslContext m => Array (AST.Pattern AST.Ann) -> m ~> m
extendWithPatterns pats = local (\ctx -> ctx { locals = foldl (flip extendEnv) ctx.locals pats })
  where
  extendEnv = TEnv <<< collectPatternVars Map.empty L.Nil

  collectPatternVars vars path = case _ of
    AST.PatWildcard _ -> vars
    AST.PatConst _ _ -> vars
    AST.PatVar _ var -> Map.insert var path vars
    AST.PatTyped _ p _ -> collectPatternVars vars path p
    AST.PatConstructor _ _ _ subpats -> collectSubpats path subpats
    AST.PatTuple _ subpats -> collectSubpats path subpats
    AST.PatAlias _ p var -> collectPatternVars (Map.insert var path vars) path p
    where
    collectSubpats path' subpats = foldlWithIndex (\i acc pat' -> collectPatternVars acc (i : path') pat') vars subpats

