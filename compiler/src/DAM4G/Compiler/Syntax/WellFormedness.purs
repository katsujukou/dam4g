module DAM4G.Syntax.WellFormedness where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (StateT, get, modify_, runStateT)
import DAM4G.Compiler.Global (GlobalDesc, Env)
import DAM4G.Compiler.Global as G
import DAM4G.Compiler.Name (GlobalName, Ident(..), OperatorName, Qualified(..))
import DAM4G.Compiler.Syntax.AST (OperatorInfo(..))
import DAM4G.Compiler.Syntax.AST as AST
import DAM4G.Compiler.Syntax.CST as CST
import DAM4G.Compiler.Syntax.Error (SyntaxError(..))
import DAM4G.Compiler.Syntax.Source (SourcePhrase, emptyLoc, (..), (@@))
import DAM4G.Compiler.Types (Associativity(..), Type_(..))
import DAM4G.Compiler.Types as T
import Data.Array (catMaybes, foldr)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, for, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)

type Expr = AST.Expr AST.Ann

type CheckState =
  { tvar :: Int
  , var :: Int
  , env :: Env
  }

initialState :: G.Env -> CheckState
initialState env =
  { tvar: 0
  , var: 0
  , env
  }

type CheckM m a = StateT CheckState (ExceptT SyntaxError m) a

type Check a = CheckM Identity a

lookupOpenedName :: forall m. Monad m => Ident -> CheckM m (Maybe (GlobalName /\ GlobalDesc))
lookupOpenedName ident = do
  { env } <- get
  pure $
    G.lookupOpenedName ident env

lookupOperator :: forall m. Monad m => OperatorName -> CheckM m (Maybe G.OperatorInfo)
lookupOperator opname = do
  { env } <- get
  pure $
    G.lookupOpenedAlias env opname

lookupType :: forall m. Monad m => Ident -> CheckM m (Maybe (GlobalName /\ G.TypeInfo))
lookupType ident = do
  { env } <- get
  pure $
    env
      # G.listOpenedType
      # Array.find (fst >>> (_ == ident))
      <#> snd

registerOperator
  :: forall m
   . Monad m
  => Qualified OperatorName
  -> GlobalName
  -> T.Associativity
  -> Int
  -> Boolean
  -> CheckM m Unit
registerOperator opname realname assoc prec opened = do
  modify_ \st -> st
    { env = G.insertOperatorAlias opname { realname, assoc, prec, opened } st.env
    }

runCheck :: forall m a. Monad m => G.Env -> CheckM m a -> m (Either SyntaxError (a /\ G.Env))
runCheck env m = runExceptT $ rmap _.env <$> runStateT m (initialState env)

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
        registerOperator opname realname assoc prec true
        pure $ Just $ OperatorInfo { loc, opname, realname, assoc, prec }
      _ -> pure Nothing

  decls <- catMaybes <$>
    for cstModule.decls case _ of
      CST.Decl a ident pats cstExp -> case NonEmptyArray.fromArray pats of
        Nothing -> do
          expr <- checkExpr cstExp
          pure $ Just $ AST.NonRec
            { ident
            , expr
            }
        Just pats' -> do
          expr <- checkExpr $ CST.ExprFunc { loc: ident.at .. a.loc } pats' cstExp
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
    >=> desugarPatternArgs
    >=> desugarMatchFn
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
    CST.ExprIdent a Nothing ident -> do
      lookupOpenedName ident >>= case _ of
        Nothing -> pure $ AST.ExprVar a ident
        Just (gloname /\ _) -> pure $ AST.ExprGlobal a gloname
    CST.ExprIdent a (Just modname) ident -> do
      pure $ AST.ExprGlobal a (Qualified modname ident)
    CST.ExprList _ _ -> throwError $ NotSupportedYet "list"
    CST.ExprTuple _ _ -> throwError $ NotSupportedYet "tuple"

    CST.ExprFunc a args body -> go body >>= flip (convertExprFunc a) (NonEmptyArray.toArray args)
    CST.ExprApp a func args -> go func >>= convertExprApp a (NonEmptyArray.toArray args)
    -- CST.ExprTyped a exp typ -> CST.ExprTyped a <$> go exp <*> pure typ
    -- CST.ExprLet a rec binds body ->
    --   CST.ExprLet a rec
    --     <$> traverse (\(CST.Binder a' pat exp) -> CST.Binder a' pat <$> go exp) binds
    --     <*> go body
    -- CST.ExprConstructor a constr args -> CST.ExprConstructor a constr <$> traverse go args
    CST.ExprIf a cond ifSo notSo -> AST.ExprIf a <$> go cond <*> go ifSo <*> go notSo
    -- CST.ExprMatch _ _ _ -> throwError $ NotSupportedYet "match expression"
    CST.ExprParensed _ exp -> go exp
    CST.ExprOperator _ _ _ -> unsafeCrashWith "Imposible"
    CST.ExprMatchFn _ _ _ -> unsafeCrashWith "Impossible"
    CST.ExprMatch a heads matrix -> AST.ExprMatch a <$> traverse go (NonEmptyArray.toArray heads) <*> convertMatchMatrix matrix
    exp -> do
      let _ = unsafePerformEffect (logShow exp)
      unsafeCrashWith "Not implemented"

  convertExprApp a args func =
    let
      go' funcExp = Array.uncons >>> case _ of
        Nothing -> pure funcExp
        Just { head: arg, tail: argRest } -> do
          funcArg <- go arg
          go' (AST.ExprApp a funcExp funcArg) argRest
    in
      go' func args

  convertExprFunc a body =
    let
      go' args = Array.uncons >>> case _ of
        Nothing -> pure $ foldr (uncurry $ AST.ExprFunc a) body args
        Just { head: arg, tail: argsRest } -> do
          case arg of
            CST.PatTyped { loc } pat' typ
              | CST.PatVar _ ident <- pat' -> do
                  typ' <- convertType typ
                  go' (Array.snoc args ((ident @@ loc) /\ Just typ')) argsRest
              | otherwise -> unsafeCrashWith "Impossible"
            CST.PatVar { loc } ident -> go' (Array.snoc args ((ident @@ loc) /\ Nothing)) argsRest
            _ -> unsafeCrashWith "Impossible"
    in
      go' []
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
      CST.PatList _ _ -> throwError $ NotSupportedYet "List pattern"
      CST.PatConstructor a constr pats -> do
        lookupOpenedName constr >>= case _ of
          Just (name /\ desc)
            | G.Constructor constrDesc <- desc -> do
                AST.PatConstructor a constrDesc.name <$> traverse go' pats
            | otherwise -> throwError $ NotAConstructor name
          Nothing -> throwError $ UnboundName constr
      CST.PatTyped a p t -> AST.PatTyped a <$> go' p <*> convertType t
      CST.PatAlias a p v -> AST.PatAlias a <$> go' p <*> pure v
      CST.PatParensed _ p -> go' p

  convertType :: CST.Type_ CST.Ann -> CheckM m (AST.Type_)
  convertType = go'
    where
    go' = case _ of
      CST.TFree { loc } ident -> do
        lookupType ident >>= case _ of
          Just (gloname /\ _) -> pure $ TGlobal (AST.AnnType $ AST.User loc) gloname
          Nothing -> throwError $ UnboundName ident
      _ -> unsafeCrashWith "not implemented type"

desugarMatchFn :: forall m. MonadRec m => CST.Expr CST.Ann -> CheckM m (CST.Expr CST.Ann)
desugarMatchFn = traverseExpr f
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
              matchHead = CST.ExprIdent emptyAnn Nothing var
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

desugarPatternArgs :: forall m. MonadRec m => CST.Expr CST.Ann -> CheckM m (CST.Expr CST.Ann)
desugarPatternArgs = traverseExpr f
  where
  f =
    case _ of
      CST.ExprFunc a pats body -> do
        let
          collectPatternArgs (acc /\ pats') = case Array.uncons pats' of
            Nothing -> pure $ Done acc
            Just { head: pat, tail: rest }
              | CST.PatTyped _ (CST.PatVar _ _) _ <- pat ->
                  pure $ Loop $ (acc { args = acc.args `Array.snoc` pat }) /\ rest
              | CST.PatVar _ _ <- pat ->
                  pure $ Loop $ (acc { args = acc.args `Array.snoc` pat }) /\ rest
              | otherwise -> do
                  var <- freshVar
                  let
                    argPat = CST.PatVar { loc: emptyLoc } var
                    argExp = CST.ExprIdent { loc: emptyLoc } Nothing var
                    next =
                      { args: Array.snoc acc.args argPat
                      , matchHeads: Array.snoc acc.matchHeads argExp
                      , matchPats: Array.snoc acc.matchPats pat
                      }
                  pure $ Loop $ next /\ rest
        { args, matchHeads, matchPats } <- tailRecM
          collectPatternArgs
          ({ args: [], matchHeads: [], matchPats: [] } /\ (NonEmptyArray.toArray pats))

        let
          funcArgs = case NonEmptyArray.fromArray args of
            Nothing -> unsafeCrashWith "Impossible"
            Just args' -> args'
          matchExpr = case NonEmptyArray.fromArray matchHeads of
            Nothing -> body
            Just heads -> CST.ExprMatch (CST.exprAnn body) heads $
              CST.PatternMatrix [ { act: body, pats: matchPats } ]
        pure
          $ CST.ExprFunc a funcArgs
          $ matchExpr
      exp -> pure exp

data OperatorAppTree
  = Leaf CST.Ann (CST.Expr CST.Ann)
  | Branch CST.Ann
      { lhs :: OperatorAppTree
      , op :: SourcePhrase () G.OperatorInfo
      , rhs :: OperatorAppTree
      }

derive instance Eq OperatorAppTree
derive instance Generic OperatorAppTree _
instance Show OperatorAppTree where
  show it = genericShow it

treeAnn :: OperatorAppTree -> CST.Ann
treeAnn = case _ of
  Leaf a _ -> a
  Branch a _ -> a

desugarOperatorAliases :: forall m. MonadRec m => CST.Expr CST.Ann -> CheckM m (CST.Expr CST.Ann)
desugarOperatorAliases = traverseExpr f
  where
  f = case _ of
    CST.ExprOperator a expHead expOps -> rebracketOperators a expHead (NonEmptyArray.toArray expOps)
    exp -> pure exp

  -- accの種類で以下を選択:
  --   accがLeafならブランチでaccを更新:
  --     acc <- (+ acc b)

  --   accがブランチなら先頭演算子(accOp) の結合性と優先順をチェック
  --     accOpのほうが優先順位が高ければ
  --     または同位でともに左結合であれば
  --       accとrhsでブランチを作る
  --         acc <- (op acc rhs)
  --     accOpのほうが優先順位が引くければ
  --     または同位でともに右結合であれば
  --       枝の付替えを行う
  --         let 
  --           accOp <- head operator of acc 
  --           accLhs <- lhs of head operator of acc
  --           accRhs <- rhs of  ゝ
  --         acc <- (accOp accLhs (op accRhs rhs))
  --     accOpとopの優先順位が同じで
  --     かつ両者の結合性が異なれば
  --       エラー.
  rebracketOperators
    :: CST.Ann
    -> CST.Expr CST.Ann
    -> Array { op :: SourcePhrase () OperatorName, rhs :: CST.Expr CST.Ann }
    -> _
  rebracketOperators _ exp exps = do
    let
      reassoc (acc /\ operators) = case Array.uncons operators of
        Nothing -> pure $ Done acc
        Just { head: { op: opname, rhs }, tail: rest } -> do
          lookupOperator opname.it >>= case _ of
            Nothing -> throwError $ UnboundOperator opname.it
            Just operatorInfo -> case acc of
              Leaf _ _ ->
                let
                  rhsAnn = CST.exprAnn rhs
                in
                  pure $ Loop $
                    Tuple
                      ( Branch
                          { loc: (treeAnn acc).loc .. rhsAnn.loc }
                          { op: operatorInfo @@ opname.at
                          , lhs: acc
                          , rhs: Leaf rhsAnn rhs
                          }
                      )
                      rest
              Branch a' accBranch@{ op: accOp, rhs: accRhs }
                | accOp.it.prec == operatorInfo.prec
                , accOp.it.assoc /= operatorInfo.assoc ->
                    throwError $
                      OperatorsAssociativityConflicts [ accOp, operatorInfo @@ opname.at ]
                | shouldAssocLeft accOp.it operatorInfo ->
                    let
                      rhsAnn = CST.exprAnn rhs
                    in
                      pure $ Loop $ Tuple
                        ( Branch
                            { loc: a'.loc .. rhsAnn.loc }
                            { op: operatorInfo @@ opname.at
                            , lhs: acc
                            , rhs: Leaf rhsAnn rhs
                            }
                        )
                        rest
                | otherwise ->
                    let
                      rhsAnn = CST.exprAnn rhs
                    in
                      pure $ Loop $ Tuple
                        ( Branch
                            { loc: a'.loc .. rhsAnn.loc }
                            ( accBranch
                                { rhs = Branch
                                    { loc: (treeAnn accRhs).loc .. rhsAnn.loc }
                                    { op: operatorInfo @@ opname.at
                                    , lhs: accRhs
                                    , rhs: Leaf rhsAnn rhs
                                    }
                                }
                            )
                        )
                        rest

    (\tree -> foldTree (let _ = unsafePerformEffect (logShow tree) in tree)) <$> tailRecM reassoc ((Leaf (CST.exprAnn exp) exp) /\ exps)

  shouldAssocLeft = case _, _ of
    { assoc: LeftAssoc, prec: prec1 }, { assoc: LeftAssoc, prec: prec2 }
      | prec1 == prec2 -> true
    { prec: prec1 }, { prec: prec2 }
      | prec1 > prec2 -> true
    _, _ -> false

  foldTree :: OperatorAppTree -> CST.Expr CST.Ann
  foldTree = case _ of
    Leaf _ exp -> exp
    Branch a { lhs, op: { at: loc, it: op }, rhs } ->
      let
        Qualified modname realname = op.realname
      in
        CST.ExprApp
          a
          (CST.ExprIdent { loc } (Just modname) realname)
          (NonEmptyArray.cons' (foldTree lhs) [ foldTree rhs ])

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
