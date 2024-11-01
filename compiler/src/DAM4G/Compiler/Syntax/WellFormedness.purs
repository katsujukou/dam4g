module DAM4G.Syntax.WellFormedness where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (StateT, get, modify, modify_, runStateT)
import DAM4G.Compiler.Global (GlobalDesc, Env)
import DAM4G.Compiler.Global as G
import DAM4G.Compiler.Name (GlobalName, Ident(..), ModuleName(..), OperatorName(..), Qualified(..))
import DAM4G.Compiler.Syntax.AST (OperatorInfo(..))
import DAM4G.Compiler.Syntax.AST as AST
import DAM4G.Compiler.Syntax.CST (PatternMatrix)
import DAM4G.Compiler.Syntax.CST as CST
import DAM4G.Compiler.Syntax.Error (ParseErrorDesc(..), SyntaxError(..))
import DAM4G.Compiler.Syntax.Source (emptyLoc, (..), (@@))
import DAM4G.Compiler.Types (Type_(..))
import DAM4G.Compiler.Types as T
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (class Traversable, foldl, for, traverse)
import Data.Tuple (fst, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)

type Expr = AST.Expr AST.Ann

type CheckState =
  { tvar :: Int
  , var :: Int
  , env :: Env
  }

initialState :: CheckState
initialState =
  { tvar: 0
  , var: 0
  , env: G.emptyEnv
  }

type CheckM m a = StateT CheckState (ExceptT SyntaxError m) a

type Check a = CheckM Identity a

lookupOpenedName :: forall m. Monad m => Ident -> CheckM m (Maybe (GlobalName /\ GlobalDesc))
lookupOpenedName ident = do
  { env } <- get
  pure $
    G.lookupOpenedName ident env

registerOperator :: forall m. Monad m => Qualified OperatorName -> GlobalName -> T.Associativity -> Int -> CheckM m Unit
registerOperator opname realname assoc prec = do
  modify_ \st -> st
    { env = G.insertOperatorAlias opname { realname, assoc, prec } st.env
    }

runCheck :: forall m a. Monad m => CheckM m a -> m (Either SyntaxError a)
runCheck m = runExceptT $ fst <$> runStateT m initialState

freshTypeVar :: forall m. Monad m => CheckM m AST.Type_
freshTypeVar = do
  { tvar } <- get
  modify_ (\st -> st { tvar = tvar + 1 })
  pure $ T.TVar (AST.AnnType AST.Compiler) $ Ident ("t" <> show tvar)

freshVars :: forall m. MonadRec m => Int -> CheckM m (Array Ident)
freshVars n
  | n <= 0 = pure []
  | otherwise = do
      let
        go (vars /\ 0) = pure $ Done vars
        go (vars /\ n') = do
          { var } <- get
          modify_ \st -> st { var = var + 1 }
          pure $ Loop $ Array.snoc vars (Ident $ "v" <> show n) /\ (n' - 1)
      tailRecM go ([] /\ n)

freshVar :: forall m. MonadRec m => CheckM m Ident
freshVar = freshVars 1 >>= case _ of
  [ ident ] -> pure ident
  _ -> unsafeCrashWith "Impossible"

checkModule :: forall m. MonadRec m => CST.Module CST.Ann -> CheckM m (AST.Module AST.Ann)
checkModule (CST.Module cstModule) = do
  -- register all operators 
  operators <- catMaybes <$>
    for cstModule.decls case _ of
      CST.DeclAlias { loc } realname' opname' { it: assoc } { it: prec } -> do
        let
          opname = Qualified cstModule.name opname'.it
          realname = Qualified cstModule.name realname'.it
        registerOperator opname realname assoc prec
        pure $ Just $ OperatorInfo { loc, opname, realname, assoc, prec }
      _ -> pure Nothing

  decls <- catMaybes <$>
    for cstModule.decls case _ of
      CST.Decl _ ident pats cstExp -> do
        expr <- checkExpr cstExp
        pure $ Just $ AST.NonRec
          { ident
          , expr
          }
      _ -> pure Nothing

  pure $ AST.Module
    { name: cstModule.name
    , loc: cstModule.loc
    , decls
    , operators
    }

checkType :: forall m. Monad m => CST.Type_ CST.Ann -> CheckM m AST.Type_
checkType _ = unsafeCrashWith "Not implemented"

checkExpr :: forall m. MonadRec m => CST.Expr CST.Ann -> CheckM m Expr
checkExpr =
  removeParens
    >=> desugarOperatorAliases
    >=> desugarMatchFn
    >=> desugarPatternArgs
    >=> convertExpr
    >=> annotateWithType

annotateWithType :: forall m. Monad m => AST.Expr CST.Ann -> CheckM m Expr
annotateWithType = traverse
  \{ loc } -> freshTypeVar >>= AST.AnnExpr loc >>> pure

convertExpr :: forall m. Monad m => CST.Expr CST.Ann -> CheckM m (AST.Expr CST.Ann)
convertExpr = go
  where
  go = case _ of
    CST.ExprConst a cst -> pure $ AST.ExprConst a cst
    CST.ExprIdent a ident -> do
      lookupOpenedName ident >>= case _ of
        Nothing -> pure $ AST.ExprVar a ident
        Just (gloname /\ _) -> pure $ AST.ExprGlobal a gloname
    CST.ExprList _ _ -> throwError $ NotSupportedYet "list"
    CST.ExprTuple _ _ -> throwError $ NotSupportedYet "tuple"

    CST.ExprFunc a args body -> convertExprFunc a (NonEmptyArray.toArray args) body
    -- CST.ExprApp a f args -> CST.ExprApp a <$> go f <*> traverse go args
    -- CST.ExprTyped a exp typ -> CST.ExprTyped a <$> go exp <*> pure typ
    -- CST.ExprLet a rec binds body ->
    --   CST.ExprLet a rec
    --     <$> traverse (\(CST.Binder a' pat exp) -> CST.Binder a' pat <$> go exp) binds
    --     <*> go body
    -- CST.ExprConstructor a constr args -> CST.ExprConstructor a constr <$> traverse go args
    CST.ExprIf a cond ifSo notSo -> AST.ExprIf a <$> go cond <*> go ifSo <*> go notSo
    -- CST.ExprMatch _ _ _ -> throwError $ NotSupportedYet "match expression"
    CST.ExprParensed a exp -> go exp
    CST.ExprOperator _ _ _ -> unsafeCrashWith "Imposible"
    CST.ExprMatchFn _ _ _ -> unsafeCrashWith "Impossible"
    CST.ExprMatch a heads matrix -> AST.ExprMatch a <$> traverse go (NonEmptyArray.toArray heads) <*> convertMatchMatrix matrix
    exp -> do
      let _ = unsafePerformEffect (logShow exp)
      unsafeCrashWith "Not implemented"

  convertExprFunc a args body =
    let
      go' = Array.uncons >>> case _ of
        Nothing -> go body
        Just { head: arg, tail: args' } -> do
          case arg of
            CST.PatTyped _ pat' typ
              | CST.PatVar _ ident <- pat' -> do
                  typ' <- convertType typ
                  AST.ExprFunc a ident typ' <$> go' args'
              | otherwise -> unsafeCrashWith "Impossible"
            pat -> throwError $ FunctionArgumentNotAnnotated (CST.patternAnn pat).loc
    in
      go' args
  convertMatchMatrix (CST.PatternMatrix matrix) = do
    AST.MatchMatrix
      <$>
        ( matrix # traverse \m -> do
            pats <- traverse convertPattern m.pats
            act <- go m.act
            pure { pats, act }
        )

  convertPattern :: CST.Pattern CST.Ann -> CheckM m (AST.Pattern CST.Ann)
  convertPattern = go'
    where
    go' = case _ of
      CST.PatWildcard a -> pure $ AST.PatWildcard a
      CST.PatConst a cst -> pure $ AST.PatConst a cst
      CST.PatVar a v -> pure $ AST.PatVar a v
      CST.PatList a pats -> throwError $ NotSupportedYet "List pattern"
      CST.PatConstructor a constr pats -> do
        lookupOpenedName constr >>= case _ of
          Just (name /\ desc)
            | G.Constructor constrDesc <- desc -> do
                AST.PatConstructor a constrDesc.name <$> traverse go' pats
            | otherwise -> throwError $ NotAConstructor name
          Nothing -> throwError $ UnboundName constr
      CST.PatTyped a p t -> AST.PatTyped a <$> go' p <*> convertType t
      CST.PatAlias a p v -> AST.PatAlias a <$> go' p <*> pure v
      CST.PatParensed a p -> go' p

  convertType :: CST.Type_ CST.Ann -> CheckM m (AST.Type_)
  convertType = go'
    where
    go' = case _ of
      CST.TFree { loc } ident -> case ident of
        Ident "Unit" -> pure $ TGlobal (AST.AnnType $ AST.User loc) (Qualified (ModuleName "Base") ident)
        Ident "Bool" -> pure $ TGlobal (AST.AnnType $ AST.User loc) (Qualified (ModuleName "Base") ident)
        Ident "Int" -> pure $ TGlobal (AST.AnnType $ AST.User loc) (Qualified (ModuleName "Base") ident)
        _ -> do
          lookupOpenedName ident >>= case _ of
            Nothing -> throwError $ UnboundName ident
            Just (gloname /\ _) -> pure $ TGlobal (AST.AnnType $ AST.User loc) gloname

      _ -> unsafeCrashWith "not implemented type"

desugarMatchFn :: forall m. MonadRec m => CST.Expr CST.Ann -> CheckM m (CST.Expr CST.Ann)
desugarMatchFn = traverseExpr f -- go
  -- where
  -- go = case _ of
  --   exp@(CST.ExprConst _ _) -> pure exp
  --   exp@(CST.ExprIdent _ _) -> pure exp
  --   CST.ExprList a exps -> CST.ExprList a <$> traverse go exps
  --   CST.ExprTuple a exps -> CST.ExprTuple a <$> traverse go exps
  --   CST.ExprOperator a exp args -> CST.ExprOperator a <$> go exp <*> traverseOperatorArgs go args
  --   CST.ExprApp a f args -> CST.ExprApp a <$> go f <*> traverse go args
  --   CST.ExprFunc a pats body -> CST.ExprFunc a pats <$> go body
  --   CST.ExprLet a rec binds body -> CST.ExprLet a rec <$> traverseBinders go binds <*> go body
  --   CST.ExprIf a cond ifSo notSo -> CST.ExprIf a <$> go cond <*> go ifSo <*> go notSo
  --   CST.ExprParensed a exp -> CST.ExprParensed a <$> go exp
  --   CST.ExprTyped a exp typ -> (\exp' -> CST.ExprTyped a exp' typ) <$> go exp
  --   CST.ExprConstructor a constr args -> CST.ExprConstructor a constr <$> traverse go args
  --   CST.ExprMatch a exps matrix -> CST.ExprMatch a <$> traverse go exps <*> traversePatternMatrix go matrix
  where
  f = case _ of
    CST.ExprMatchFn a pats matrix -> do
      let
        mkFuncArgs (args /\ heads) = Array.uncons >>> case _ of
          Nothing -> pure $ args /\ heads
          Just { head: p, tail: ps } -> do
            var <- freshVar
            let
              emptyAnn = { loc: emptyLoc }
              arg = case p of
                CST.PatTyped a' p' typ -> CST.PatTyped a' (CST.PatVar emptyAnn var) typ
                _ -> CST.PatVar emptyAnn var
              matchHead = CST.ExprIdent emptyAnn var
            mkFuncArgs (Array.snoc args arg /\ Array.snoc heads matchHead) ps

      args /\ matchHeads <- mkFuncArgs ([] /\ []) (NonEmptyArray.toArray pats)
        <#> uncurry case _, _ of
          ps, exps
            | Just pats' <- NonEmptyArray.fromArray ps
            , Just exps' <- NonEmptyArray.fromArray exps -> pats' /\ exps'
          _, _ -> unsafeCrashWith "Impossible"

      desugaredMatrix <- traversePatternMatrix (traverseExpr f) matrix

      pure $ CST.ExprFunc a args (CST.ExprMatch a matchHeads desugaredMatrix)
    exp -> pure exp

desugarPatternArgs :: forall m. Monad m => CST.Expr CST.Ann -> CheckM m (CST.Expr CST.Ann)
desugarPatternArgs = go
  where
  go = case _ of
    exp -> pure exp

desugarOperatorAliases :: forall m. Monad m => CST.Expr CST.Ann -> CheckM m (CST.Expr CST.Ann)
desugarOperatorAliases = traverseExpr f -- go
  where
  f = case _ of
    exp -> pure exp

-- where
-- go = case _ of
--   CST.ExprList _ _ -> throwError $ NotSupportedYet "list"
--   CST.ExprTuple _ _ -> throwError $ NotSupportedYet "tuple"
--   CST.ExprFunc a args body -> CST.ExprFunc a args <$> go body
--   CST.ExprApp a f args -> CST.ExprApp a <$> go f <*> traverse go args
--   CST.ExprOperator a hd tl -> CST.ExprOperator a <$> go hd <*> traverse (\{ op, rhs } -> { op, rhs: _ } <$> go rhs) tl
--   CST.ExprLet a rec binds body ->
--     CST.ExprLet a rec
--       <$> traverse (\(CST.Binder a' pat exp) -> CST.Binder a' pat <$> go exp) binds
--       <*> go body
--   CST.ExprConstructor a constr args -> CST.ExprConstructor a constr <$> traverse go args
--   CST.ExprIf a cond ifSo notSo -> CST.ExprIf a <$> go cond <*> go ifSo <*> go notSo
--   CST.ExprMatch _ _ _ -> throwError $ NotSupportedYet "match expression"
--   CST.ExprMatchFn _ _ _ -> throwError $ NotSupportedYet "matchfn expression"
--   CST.ExprTyped a exp typ -> CST.ExprTyped a <$> go exp <*> pure typ
--   CST.ExprParensed a exp -> CST.ExprParensed a <$> go exp
--   exp -> pure exp

removeParens :: forall m a. Monad m => CST.Expr a -> CheckM m (CST.Expr a)
removeParens = traverseExpr f
  where
  f = case _ of
    CST.ExprParensed a e -> f e
    CST.ExprFunc a pats body -> CST.ExprFunc a <$> traverse goPat pats <*> f body
    CST.ExprMatchFn a pats matrix -> CST.ExprMatchFn a <$> traverse goPat pats <*> traversePatternMatrix f matrix
    CST.ExprTyped a e t -> CST.ExprTyped a <$> f e <*> goTyp t
    exp -> pure exp

  goPat :: forall a'. CST.Pattern a' -> CheckM m (CST.Pattern a')
  goPat = case _ of
    CST.PatParensed _ p -> pure p
    CST.PatTyped a p t -> CST.PatTyped a <$> goPat p <*> goTyp t
    CST.PatList a pats -> CST.PatList a <$> traverse goPat pats
    CST.PatAlias a p name -> CST.PatAlias a <$> goPat p <*> pure name
    CST.PatConstructor a constr pats -> CST.PatConstructor a constr <$> traverse goPat pats
    pat -> pure pat

  goTyp :: forall a'. CST.Type_ a' -> CheckM m (CST.Type_ a')
  goTyp = case _ of
    CST.TFunc a argTyps retTyp -> CST.TFunc a <$> traverse goTyp argTyps <*> goTyp retTyp
    CST.TParens a t -> goTyp t
    typ -> pure typ

traverseOperatorArgs
  :: forall m t op a b
   . Monad m
  => Traversable t
  => (a -> CheckM m b)
  -> t { op :: op, rhs :: a }
  -> CheckM m (t { op :: op, rhs :: b })
traverseOperatorArgs f = traverse (\{ op, rhs } -> { op, rhs: _ } <$> f rhs)

traverseBinders
  :: forall m t a
   . Monad m
  => Traversable t
  => (CST.Expr a -> CheckM m (CST.Expr a))
  -> t (CST.Binder a)
  -> CheckM m (t (CST.Binder a))
traverseBinders f = traverse (\(CST.Binder a pat exp) -> CST.Binder a pat <$> f exp)

traversePatternMatrix
  :: forall m a
   . Monad m
  => (CST.Expr a -> CheckM m (CST.Expr a))
  -> CST.PatternMatrix a
  -> CheckM m (CST.PatternMatrix a)
traversePatternMatrix f (CST.PatternMatrix matrix) = CST.PatternMatrix <$> traverse (\({ pats, act }) -> { pats, act: _ } <$> f act) matrix

traverseExpr :: forall m a. Monad m => (CST.Expr a -> CheckM m (CST.Expr a)) -> CST.Expr a -> CheckM m (CST.Expr a)
traverseExpr f = go
  where
  go = case _ of
    CST.ExprList a exps -> f =<< CST.ExprList a <$> traverse go exps
    CST.ExprTuple a exps -> f =<< CST.ExprTuple a <$> traverse go exps
    CST.ExprFunc a pats body -> f =<< CST.ExprFunc a pats <$> go body
    CST.ExprLet a rec binds body -> f =<< CST.ExprLet a rec <$> traverseBinders go binds <*> go body
    CST.ExprApp a func args -> f =<< f =<< CST.ExprApp a <$> go func <*> traverse go args
    CST.ExprOperator a exp args -> f =<< CST.ExprOperator a <$> go exp <*> traverseOperatorArgs go args
    CST.ExprIf a cond ifSo notSo -> f =<< CST.ExprIf a <$> go cond <*> go ifSo <*> go notSo
    CST.ExprParensed a exp -> f =<< CST.ExprParensed a <$> go exp
    CST.ExprTyped a exp typ -> f =<< (\exp' -> CST.ExprTyped a exp' typ) <$> go exp
    CST.ExprConstructor a constr args -> f =<< CST.ExprConstructor a constr <$> traverse go args
    CST.ExprMatch a exps matrix -> f =<< CST.ExprMatch a <$> traverse go exps <*> traversePatternMatrix go matrix
    CST.ExprMatchFn a pats matrix -> f =<< CST.ExprMatchFn a pats <$> traversePatternMatrix f matrix
    exp -> f exp
