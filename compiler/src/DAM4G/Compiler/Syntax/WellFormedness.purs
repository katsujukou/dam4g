module DAM4G.Syntax.WellFormedness where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (StateT, get, modify_, runStateT)
import DAM4G.Compiler.Global (GlobalDesc, Env)
import DAM4G.Compiler.Global as G
import DAM4G.Compiler.Name (ConstructorName(..), GlobalName, Ident(..), ModuleName(..), NameSource(..), OperatorName, Qualified(..), TypeName(..), identToConstructorName, identToTypeName, unqualify, upperIdentRegex)
import DAM4G.Compiler.Name as N
import DAM4G.Compiler.Syntax.AST (SourceIdent)
import DAM4G.Compiler.Syntax.AST as AST
import DAM4G.Compiler.Syntax.CST (patternAnn)
import DAM4G.Compiler.Syntax.CST as CST
import DAM4G.Compiler.Syntax.Error (SyntaxError(..))
import DAM4G.Compiler.Syntax.Source (SourcePhrase, emptyLoc, (..), (@@))
import DAM4G.Compiler.Types (Associativity(..), ConstructorTag)
import DAM4G.Compiler.Types as T
import Data.Array (catMaybes, foldr, mapWithIndex)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex as Re
import Data.Traversable (class Traversable, for, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)

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

lookupConstructor
  :: forall m
   . Monad m
  => Qualified ConstructorName
  -> CheckM m (Maybe G.ConstructorDesc)
lookupConstructor constr = do
  { env } <- get
  pure $
    env.globals # Map.lookup (coerce constr) >>= _.desc >>> case _ of
      G.Constructor desc -> Just desc
      _ -> Nothing

lookupOperator :: forall m. Monad m => OperatorName -> CheckM m (Maybe G.OperatorInfo)
lookupOperator opname = do
  { env } <- get
  pure $
    G.lookupOpenedAlias env opname

lookupType :: forall m. Monad m => Qualified TypeName -> CheckM m (Maybe G.TypeInfo)
lookupType gtypname = do
  { env } <- get
  pure $
    env.types
      # Map.lookup gtypname

lookupOpenedConstructor
  :: forall m
   . Monad m
  => ConstructorName
  -> CheckM m (Maybe G.ConstructorDesc)
lookupOpenedConstructor constr = do
  { env } <- get
  pure $
    env
      # G.listOpenedDecls
      # Array.find (fst >>> unqualify >>> (_ == coerce constr))
      >>=
        ( snd >>> \info -> case info.desc of
            G.Constructor desc -> Just desc
            _ -> Nothing
        )

lookupOpenedType
  :: forall m
   . Monad m
  => TypeName
  -> CheckM m (Maybe (Qualified TypeName /\ G.TypeInfo))
lookupOpenedType typname = do
  { env } <- get
  pure $
    env
      # G.listOpenedType
      # Array.find (fst >>> unqualify >>> (_ == typname))

registerType
  :: forall m
   . Monad m
  => Qualified TypeName
  -> T.Kind Unit
  -> Array (Qualified ConstructorName)
  -> CheckM m Unit
registerType typename kind constrs = do
  modify_ \st -> st
    { env = G.insertType typename { kind, opened: true, constrs } st.env }

registerDecl
  :: forall m
   . Monad m
  => GlobalName
  -> G.GlobalInfo
  -> CheckM m Unit
registerDecl name info = do
  modify_ \st -> st
    { env = G.insertDecl (coerce name) info st.env }

registerOperator
  :: forall m
   . Monad m
  => Qualified OperatorName
  -> GlobalName
  -> T.Associativity
  -> Int
  -> CheckM m Unit
registerOperator opname realname assoc prec = do
  modify_ \st -> st
    { env = G.insertOperatorAlias opname { realname, assoc, prec, opened: true } st.env
    }

runCheck :: forall m a. Monad m => G.Env -> CheckM m a -> m (Either SyntaxError (a /\ G.Env))
runCheck env m = runExceptT $ rmap _.env <$> runStateT m (initialState env)

freshTypeVar :: forall m. Monad m => CheckM m AST.Type_
freshTypeVar = do
  { tvar } <- get
  modify_ (\st -> st { tvar = tvar + 1 })
  pure $ T.TVar { src: Compiler } $ Ident ("t" <> show tvar)

freshVars :: forall m. Monad m => Int -> CheckM m (Array Ident)
freshVars n
  | n <= 0 = pure []
  | otherwise = do
      let
        go vars 0 = pure vars
        go vars n' = do
          { var } <- get
          modify_ \st -> st { var = var + 1 }
          go (Array.snoc vars (Ident $ "v" <> show var)) (n' - 1)
      go [] n

freshVar :: forall m. MonadRec m => CheckM m Ident
freshVar = freshVars 1 >>= case _ of
  [ ident ] -> pure ident
  _ -> unsafeCrashWith "Impossible"

checkModule :: forall m. MonadRec m => CST.Module CST.Ann -> CheckM m (AST.Module AST.Ann)
checkModule (CST.Module cstModule) = do
  -- register all operators
  operators <- catMaybes <$>
    for cstModule.decls
      case _ of
        -- 本当はここでエイリアスのチェックを行うべき
        CST.DeclAlias { loc } realname' opname' { it: assoc } { it: prec } -> do
          let
            opname = Qualified cstModule.name opname'.it
            realname = Qualified cstModule.name realname'.it
          registerOperator opname realname assoc prec
          pure $ Just $ AST.OperatorInfo { loc, opname, realname, assoc, prec }
        _ -> pure Nothing

  AST.Module <$> ado
    typeDecls <- checkTypeDecls cstModule.decls
    decls <- checkDecls cstModule.decls
    in
      { name: cstModule.name
      , loc: cstModule.loc
      , operators
      , decls
      , typeDecls
      }
  where
  qualify :: forall a. a -> Qualified a
  qualify = Qualified cstModule.name

  checkTypeDecls decls = catMaybes <$>
    for decls case _ of
      CST.DeclSet { loc } name cs
        | Just typname' <- identToTypeName name.it -> do
            let typname = qualify typname'
            constrs <- forWithIndex cs \i c -> do
              let constr = CST.nameOfConstructor c
              case identToConstructorName constr.it of
                Nothing -> throwError $ InvalidConstructorName constr.at constr.it
                Just constrName -> do
                  c' <- checkConstr typname i c
                  registerDecl (qualify $ coerce constrName)
                    { opened: true
                    , desc: G.Constructor c'
                    , typ: T.TGlobal unit c'.typname
                    }
                  pure c'

            registerType typname (T.KndType unit) ((coerce <<< _.name) <$> constrs)

            pure $ Just $ AST.TypeDeclaration
              { loc
              , typname
              , kind: T.KndType unit
              , constrs: constrs # mapWithIndex \i c -> AST.TypeDeclarationConstructor
                  { name: c.name
                  , tag: i
                  , argTypes: c.argTypes
                  }
              }
        | otherwise -> throwError $ InvalidTypName name.at name.it
      _ -> pure Nothing

  checkConstr :: Qualified TypeName -> ConstructorTag -> CST.Constructor CST.Ann -> _ G.ConstructorDesc
  checkConstr typname tag = case _ of
    CST.ConstrNull a ident -> doCheck a ident []
    CST.ConstrStructured a ident argType -> do
      checkType argType
        >>= case _ of
          T.TTup _ typs -> doCheck a ident (map (const unit) <$> typs)
          t -> doCheck a ident [ const unit <$> t ]
    where
    doCheck :: CST.Ann -> SourcePhrase () Ident -> Array (T.Type_ Unit) -> CheckM m G.ConstructorDesc
    doCheck _ ident argTypes = case identToConstructorName ident.it of
      Nothing -> throwError $ InvalidConstructorName ident.at ident.it
      Just constrName -> do
        lookupOpenedConstructor (coerce constrName) >>= case _ of
          Just _ -> throwError $ ConstructorNameConflicts ident.at constrName
          _ -> pure
            { name: qualify constrName
            , typname
            , tag
            , argTypes
            }
  checkDecls decls = catMaybes <$>
    for decls case _ of
      CST.Decl a ident pats cstExp -> do
        expr <- case NonEmptyArray.fromArray pats of
          Nothing -> checkExpr cstExp
          Just pats' -> checkExpr $ CST.ExprFunc { loc: ident.at .. a.loc } pats' cstExp
        let
          decl' = AST.NonRec { ident, expr }
          typ = case AST.exprAnn expr of
            AST.AnnExpr _ t -> const unit <$> t

        registerDecl (qualify ident.it)
          { desc: G.Normal {}
          , typ
          , opened: true
          }

        pure $ Just $ decl'

      _ -> pure Nothing

checkType :: forall m. Monad m => CST.Type_ CST.Ann -> CheckM m (T.Type_ T.TypeAnn)
checkType = case _ of
  CST.TIdent a Nothing ident
    | Just typname <- identToTypeName ident -> do
        lookupOpenedType typname >>= case _ of
          -- #TODO: 再帰型はどうやってチェックする？
          Nothing -> throwError $ UnboundTypeName typname
          Just (gtypname /\ _) -> pure $ T.TGlobal { src: User a.loc } gtypname
    | otherwise -> pure $ T.TVar { src: User a.loc } ident
  CST.TIdent a (Just modname) ident
    | Just typename <- identToTypeName ident -> do
        let gtypname = Qualified modname typename
        lookupType gtypname >>= case _ of
          Just _ -> pure $ T.TGlobal { src: User a.loc } gtypname
          Nothing -> do
            let (Qualified modname' ident') = gtypname
            throwError $ ModuleDoesNotExportName modname' (coerce ident')
  CST.TTup a typs -> T.TTup { src: User a.loc } <$> traverse checkType typs
  CST.TFunc a argTyps retTyp -> do
    let
      go typs = Array.uncons >>> case _ of
        Nothing -> pure typs
        Just { head, tail: rest } -> do
          argTyp <- checkType head
          go (typs `Array.snoc` argTyp) rest
    ret <- checkType retTyp
    typs <- go [] $ NonEmptyArray.toArray argTyps
    pure $ typs #
      flip Array.foldr ret
        ( \typ acc -> do
            let
              a' = case (T.typeAnn typ).src, (T.typeAnn acc).src of
                N.User loc1, N.User loc2 -> { src: N.User (loc1 .. loc2) }
                N.User loc, N.Compiler -> { src: N.User loc }
                N.Compiler, N.User loc -> { src: N.User loc }
                _, _ -> { src: N.Compiler }
            T.TFunc a' typ acc
        )
  t -> let _ = unsafePerformEffect (logShow t) in unsafeCrashWith "Not implemented"

checkPattern :: forall m. Monad m => CST.Pattern CST.Ann -> CheckM m (AST.Pattern CST.Ann)
checkPattern = go'
  where
  go' = case _ of
    CST.PatWildcard a -> pure $ AST.PatWildcard a
    CST.PatConst a cst -> pure $ AST.PatConst a cst
    CST.PatVar a v -> pure $ AST.PatVar a v
    CST.PatList _ _ -> throwError $ NotSupportedYet "List pattern"
    CST.PatTuple a pats -> AST.PatTuple a <$> traverse go' pats
    CST.PatConstructor a constrMod constr mbPat
      | Just constrname <- identToConstructorName constr -> do
          mbConstrDesc <- case constrMod of
            Nothing -> lookupOpenedConstructor constrname
            Just modname' -> lookupConstructor (Qualified modname' constrname)
          case mbConstrDesc of
            Nothing -> throwError $ UnknownConstructor constrMod constrname
            Just desc -> traverse go' mbPat >>= case _ of
              Just pat
                | AST.PatTuple _ pats <- pat -> pure $ AST.PatConstructor a desc.typname desc.tag pats
                | otherwise -> pure $ AST.PatConstructor a desc.typname desc.tag [ pat ]
              Nothing -> pure $ AST.PatConstructor a desc.typname desc.tag []
      | otherwise -> throwError $ NotAConstructor Nothing constr
    CST.PatTyped a p t -> AST.PatTyped a <$> go' p <*> checkType t
    CST.PatAlias a p v -> AST.PatAlias a <$> go' p <*> pure v
    CST.PatParensed _ p -> go' p

checkExpr :: forall m. MonadRec m => CST.Expr CST.Ann -> CheckM m Expr
checkExpr =
  desugarPatternArgs
    >=> desugarMatchFn
    >=> desugarLetBinding
    >=> desugarOperatorAliases
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
    CST.ExprIdent a qual ident
      | upperIdentRegex `Re.test` (coerce ident) -> do
          mbConstr <- case qual of
            Nothing -> do
              lookupOpenedConstructor (coerce ident)
            Just modname -> do
              lookupConstructor (Qualified modname $ coerce ident)
          case mbConstr of
            Nothing -> throwError $ UnknownConstructor qual (coerce ident)
            Just constr
              | [] <- constr.argTypes -> pure $ AST.ExprConstructor a constr.name []
              | otherwise -> pure $ AST.ExprGlobal a (coerce constr.name)
      | otherwise -> do
          lookupOpenedName ident >>= case _ of
            Nothing -> pure $ AST.ExprVar a ident
            Just (gloname /\ _) -> pure $ AST.ExprGlobal a gloname
    CST.ExprList _ _ -> throwError $ NotSupportedYet "list"
    CST.ExprTuple a exps -> AST.ExprTuple a <$> traverse go exps
    CST.ExprFunc a args body -> go body >>= flip (convertExprFunc a) (NonEmptyArray.toArray args)
    CST.ExprApp a func args -> go func >>= convertExprApp a (NonEmptyArray.toArray args)
    CST.ExprTyped a exp typ -> do
      AST.ExprTyped a <$> go exp <*> checkType typ
    CST.ExprLet a CST.NonRec binds body
      | [ binder ] <- NonEmptyArray.toArray binds
      , CST.Binder a (CST.PatVar va name) exp1 <- binder -> do
          AST.ExprLet a (name @@ va.loc) <$> go exp1 <*> go body
      | otherwise -> unsafeCrashWith "Impossible"
    CST.ExprIf a cond ifSo notSo -> AST.ExprIf a <$> go cond <*> go ifSo <*> go notSo
    CST.ExprParensed _ exp -> go exp
    CST.ExprOperator _ _ _ -> unsafeCrashWith "Imposible"
    CST.ExprMatchFn _ _ _ -> unsafeCrashWith "Impossible"
    CST.ExprMatch a heads matrix -> AST.ExprMatch a <$> convertMatchMatrix (NonEmptyArray.toArray heads) matrix
    exp -> do
      let _ = unsafePerformEffect (logShow exp)
      unsafeCrashWith "Not implemented"

  convertExprApp a args func =
    let
      go' funcExp = Array.uncons >>> case _ of
        Nothing -> pure funcExp
        Just { head: arg, tail: argRest } -> do
          funcArg <- go arg
          app <- case funcExp of
            AST.ExprGlobal _ gloname -> do
              lookupConstructor (coerce gloname) >>= case _ of
                Just constr -> case funcArg of
                  AST.ExprTuple _ exps -> pure $ AST.ExprConstructor (AST.exprAnn funcExp) constr.name exps
                  _ -> pure $ AST.ExprConstructor (AST.exprAnn funcExp) constr.name [ funcArg ]
                Nothing -> pure (AST.ExprApp a funcExp funcArg)
            _ -> pure (AST.ExprApp a funcExp funcArg)
          go' app argRest
    in
      go' func args

  convertExprFunc a body =
    let
      go' args =
        let
          _ = unsafePerformEffect (logShow args)
        in
          Array.uncons >>> case _ of
            Nothing -> pure $ foldr (uncurry $ AST.ExprFunc a) body args
            Just { head: arg, tail: argsRest } -> do
              case arg of
                CST.PatTyped { loc } pat' typ
                  | CST.PatVar _ ident <- pat' -> do
                      typ' <- checkType typ
                      go' (Array.snoc args ((ident @@ loc) /\ Just typ')) argsRest
                  | otherwise -> unsafeCrashWith "Impossible"
                CST.PatVar { loc } ident -> go' (Array.snoc args ((ident @@ loc) /\ Nothing)) argsRest
                _ -> unsafeCrashWith "Impossible"
    in
      go' []

  convertMatchMatrix matchHeads (CST.PatternMatrix matrix') = do
    heads <- traverse go matchHeads
    matrix <- matrix' # traverse \m -> do
      pats <- traverse checkPattern m.pats
      act <- go m.act
      pure { pats, act }
    pure $ AST.MatchMatrix
      { heads
      , matrix
      }

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
                CST.PatTyped a' p' typ -> CST.PatTyped a' (CST.PatVar (patternAnn p') var) typ
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
          collectPatternArgs acc = Array.uncons >>> case _ of
            Nothing -> pure acc
            Just { head: pat, tail: rest }
              | CST.PatTyped _ (CST.PatVar _ _) _ <- pat ->
                  collectPatternArgs (acc { args = acc.args `Array.snoc` pat }) rest
              | CST.PatVar _ _ <- pat ->
                  collectPatternArgs (acc { args = acc.args `Array.snoc` pat }) rest
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
                  collectPatternArgs next rest
        { args, matchHeads, matchPats } <- collectPatternArgs
          { args: [], matchHeads: [], matchPats: [] }
          (NonEmptyArray.toArray pats)

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
desugarOperatorAliases = traverseExpr'
  (\a mod id -> pure $ CST.ExprIdent a mod id)
  pure
  f
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

emptyAnn :: CST.Ann
emptyAnn = { loc: emptyLoc }

desugarLetBinding
  :: forall m
   . MonadRec m
  => CST.Expr CST.Ann
  -> CheckM m (CST.Expr CST.Ann)
desugarLetBinding = traverseExpr f
  where
  f = case _ of
    CST.ExprLet a CST.NonRec binders body -> letToMatch body (NonEmptyArray.toArray binders)
    exp -> pure exp

  letToMatch body = go
    where
    go :: Array (CST.Binder CST.Ann) -> CheckM m _
    go = Array.uncons >>> case _ of
      Nothing -> pure body
      Just { head: binder@(CST.Binder ba pat exp), tail: bindRest } ->
        case pat of
          -- 変数への束縛 (let x = e in ...)の場合は何もしない
          CST.PatVar _ _ -> do
            body' <- go bindRest
            pure $
              CST.ExprLet emptyAnn CST.NonRec
                (NonEmptyArray.singleton binder)
                body'

          -- destructuring (let Box n = e in ...等)の場合、パターンマッチにdesugarする
          _ -> do
            body' <- go bindRest
            name <- freshVar
            pure $
              CST.ExprLet emptyAnn CST.NonRec
                ( NonEmptyArray.singleton
                    (CST.Binder ba (CST.PatVar emptyAnn name) exp)
                )
                ( CST.ExprMatch emptyAnn
                    (NonEmptyArray.singleton (CST.ExprIdent emptyAnn Nothing name))
                    ( CST.PatternMatrix
                        [ { pats: [ pat ], act: body' }
                        ]
                    )
                )

removeParens :: forall m a. Monad m => CST.Expr a -> CheckM m (CST.Expr a)
removeParens = traverseExpr f
  where
  f = case _ of
    CST.ExprParensed _ e -> f e
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
    CST.PatConstructor a constrMod constr pats -> CST.PatConstructor a constrMod constr <$> traverse goPat pats
    pat -> pure pat

  goTyp :: forall a'. CST.Type_ a' -> CheckM m (CST.Type_ a')
  goTyp = case _ of
    CST.TFunc a argTyps retTyp -> CST.TFunc a <$> traverse goTyp argTyps <*> goTyp retTyp
    CST.TParens _ t -> goTyp t
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
    CST.ExprMatch a exps matrix -> f =<< CST.ExprMatch a <$> traverse go exps <*> traversePatternMatrix go matrix
    CST.ExprMatchFn a pats matrix -> f =<< CST.ExprMatchFn a pats <$> traversePatternMatrix f matrix
    exp -> f exp

traverseExpr'
  :: forall m a
   . Monad m
  => (a -> Maybe ModuleName -> Ident -> CheckM m (CST.Expr a))
  -> (CST.Type_ a -> CheckM m (CST.Type_ a))
  -> (CST.Expr a -> CheckM m (CST.Expr a))
  -> CST.Expr a
  -> CheckM m (CST.Expr a)
traverseExpr' onIdent onType f = go
  where
  go = case _ of
    exp@(CST.ExprConst _ _) -> pure exp
    CST.ExprIdent a modname ident -> onIdent a modname ident
    CST.ExprList a exps -> f =<< CST.ExprList a <$> traverse go exps
    CST.ExprTuple a exps -> f =<< CST.ExprTuple a <$> traverse go exps
    CST.ExprFunc a pats body -> f =<< CST.ExprFunc a pats <$> go body
    CST.ExprLet a rec binds body -> f =<< CST.ExprLet a rec <$> traverseBinders go binds <*> go body
    CST.ExprApp a func args -> f =<< f =<< CST.ExprApp a <$> go func <*> traverse go args
    CST.ExprOperator a exp args -> f =<< CST.ExprOperator a <$> go exp <*> traverseOperatorArgs go args
    CST.ExprIf a cond ifSo notSo -> f =<< CST.ExprIf a <$> go cond <*> go ifSo <*> go notSo
    CST.ExprParensed a exp -> f =<< CST.ExprParensed a <$> go exp
    CST.ExprTyped a exp typ -> f =<< (\exp' -> CST.ExprTyped a exp' typ) <$> go exp
    CST.ExprMatch a exps matrix -> f =<< CST.ExprMatch a <$> traverse go exps <*> traversePatternMatrix go matrix
    CST.ExprMatchFn a pats matrix -> f =<< CST.ExprMatchFn a pats <$> traversePatternMatrix f matrix
