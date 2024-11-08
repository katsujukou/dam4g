module DAM4G.Compiler.Optimizer.Translate where

import Prelude

import Control.Monad.List.Trans (foldl)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (get, put, runStateT)
import DAM4G.Compiler.Global as G
import DAM4G.Compiler.Name (Qualified(..))
import DAM4G.Compiler.Optimizer.IR (ELC(..))
import DAM4G.Compiler.Optimizer.IR as IR
import DAM4G.Compiler.Optimizer.Translate.MatchComp as MatchComp
import DAM4G.Compiler.Optimizer.Translate.Monad (TranslEnv(..), TranslM, TranslState, extendWithNewVar, extendWithPatterns, getConstructorTag, lookupDecl, searchIdent)
import DAM4G.Compiler.Primitive (Primitive(..), isArithmetic, isLogical)
import DAM4G.Compiler.Syntax.AST as AST
import DAM4G.Compiler.Types (BlockTag(..), StructuredConstant(..))
import Data.Array ((!!))
import Data.Array as Array
import Data.List (foldr)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, for, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Partial.Unsafe (unsafeCrashWith)

foreach :: forall t m a b. Monad m => Traversable t => t a -> (a -> TranslM m b) -> TranslM m (t b)
foreach as f = do
  current <- get
  bs <- for as \a -> do
    put current
    f a
  put current $> bs

runTranslM :: forall m a. Monad m => G.Env -> TranslM m a -> m (a /\ TranslState)
runTranslM genv m = ctx # runReaderT (runStateT m { genv })
  where
  ctx =
    { locals: TNull
    }

translate :: forall m. Monad m => G.Env -> AST.Module AST.Ann -> m (IR.Module /\ TranslState)
translate genv m = runTranslM genv (translModule m)

translModule :: forall m. Monad m => AST.Module AST.Ann -> TranslM m IR.Module
translModule (AST.Module astModule) = do
  decls <- for astModule.decls \decl -> translDecl decl
  pure $ IR.Module
    { name: astModule.name
    , decls
    }

translDecl :: forall m. Monad m => AST.Declaration AST.Ann -> TranslM m IR.Declaration
translDecl decl = do
  case decl of
    AST.NonRec { ident, expr } -> IR.NonRec <<< IR.Decl <<< { ident: ident.it, term: _ } <$> translExpr expr
    AST.Rec decls -> unsafeCrashWith "Not implemented: rec decl"

translExpr :: forall m. Monad m => AST.Expr AST.Ann -> TranslM m (ELC IR.Ann)
translExpr = go
  where
  go = case _ of
    AST.ExprConst _ cst -> pure $ ELConst unit (SCAtom cst)

    AST.ExprVar _ ident -> do
      var /\ path <- searchIdent ident
      pure $ foldr (\o lam -> ELPrim unit (PField o) [ lam ]) (ELVar unit var) path

    AST.ExprGlobal _ gloname -> do
      lookupDecl gloname >>= case _ of
        { desc }
          | G.Prim primDesc <- desc -> do
              pure $ ELPrim unit primDesc.prim []
          | G.Constructor constrDesc <- desc -> do
              tag <- getConstructorTag constrDesc.name
              pure $ ELPrim unit (PMakeBlock (TConstr tag)) []
          | otherwise -> do
              let Qualified modname' ident' = gloname
              pure $ ELPrim unit (PGetGlobal modname' ident') []

    AST.ExprTuple _ exps -> ELPrim unit (PMakeBlock TTuple) <$> traverse go exps
    AST.ExprConstructor _ constrName args -> do
      tag <- getConstructorTag constrName
      trArgs <- traverse go args
      pure $ ELPrim unit (PMakeBlock (TConstr tag)) $ trArgs
    AST.ExprField _ exp n -> do
      go exp >>= case _ of
        term -> pure $ ELPrim unit (PField n) [ term ]

    exp@(AST.ExprFunc _ _ _ _) -> translExprFunc exp
    exp@(AST.ExprApp _ _ _) -> translExprApp exp

    AST.ExprLet _ { it: name } exp body -> translExprLet name exp body

    AST.ExprIf _ cond ifSo notSo -> IR.ELIf unit <$> go cond <*> go ifSo <*> go notSo
    AST.ExprMatch _ mm -> translExprMatch mm
    AST.ExprTyped _ exp _ -> go exp

  translExprFunc = transl 0
    where
    transl arity = case _ of
      AST.ExprFunc _ { it: arg } _ body -> do
        extendWithNewVar (spy "arg" arg) L.Nil do
          transl (arity + 1) body
      exp' -> do
        body <- go exp'
        pure $ ELAbs unit arity body

  translExprApp :: AST.Expr _ -> TranslM m (IR.ELC IR.Ann)
  translExprApp = transl []
    where
    transl args = case _ of
      AST.ExprApp _ func arg -> do
        trArg <- go arg
        transl (Array.cons trArg args) func
      exp -> do
        go exp >>= case _ of
          IR.ELPrim a prim _
            | PMakeBlock _ <- prim -> pure $ IR.ELPrim a prim args
            | isArithmetic prim -> pure $ IR.ELPrim a prim args
            | isLogical prim -> pure $ IR.ELPrim a prim args
          term -> pure $ ELApp unit term args

  translExprLet name exp body = do
    tmArg <- go exp
    tmBody <- extendWithNewVar name L.Nil (go body)
    pure $ squashLet [] (IR.ELLet unit [ tmArg ] tmBody)
    where
    squashLet tmArgs = case _ of
      IR.ELLet _ tmArgs' tmBody -> squashLet (tmArgs <> tmArgs') tmBody
      tmBody -> IR.ELLet unit tmArgs tmBody
  translExprMatch mm@(AST.MatchMatrix { matrix }) = do
    MatchComp.composeDecisionTree mm >>= foldTree
    where
    foldTree :: MatchComp.DecisionTree AST.Ann -> TranslM m (IR.ELC IR.Ann)
    foldTree = case _ of
      MatchComp.Leaf i -> case matrix !! i of
        Nothing -> unsafeCrashWith "Impossible"
        Just { pats, act: exp } -> extendWithPatterns pats (go exp)
      MatchComp.Fail -> pure $ ELRaise unit
      MatchComp.Choice tree1 tree2 -> ELTrap unit <$> foldTree tree1 <*> foldTree tree2
      MatchComp.Conditional head tbl -> ELCond unit <$> go head <*> traverse (\(cst /\ tree') -> (/\) cst <$> foldTree tree') tbl
      MatchComp.JumpThru head tbl -> ELSwitch unit <$> go head <*> traverse (\(tag /\ tree') -> (/\) tag <$> foldTree tree') tbl
