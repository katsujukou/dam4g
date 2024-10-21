module DAM4G.Compiler.Backend.Lower where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (StateT, get, modify_, put, runStateT)
import DAM4G.Compiler.Backend.CodeLabel (CodeLabel(..))
import DAM4G.Compiler.Backend.Instruction (Instruction(..))
import DAM4G.Compiler.Backend.Program (CodeSection(..), Program(..), codeLength, empty)
import DAM4G.Compiler.Name (Ident(..), ModuleName)
import DAM4G.Compiler.Optimizer.IR (ELC(..), Primitive(..), Var(..))
import DAM4G.Compiler.Optimizer.IR as IR
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)

type ELTerm = IR.ELC IR.Ann

type LowerState =
  { moduleName :: ModuleName
  , init :: Array Instruction
  , next :: Int
  , more :: List { lbl :: CodeLabel, arity :: Int, term :: ELTerm }
  }

type LowerM a = StateT LowerState Identity a

runLowerM :: forall a. LowerState -> LowerM a -> a /\ LowerState
runLowerM s0 m = case runStateT m s0 of
  Identity res -> res

lower :: IR.Module -> Program
lower m@(IR.Module { name }) = fst $ runLowerM initialState $ compileModule m
  where
  initialState =
    { moduleName: name
    , next: 0
    , init: []
    , more: List.Nil
    }

isTail :: List Instruction -> Boolean
isTail = case _ of
  KReturn : _ -> true
  KStop : _ -> true
  _ -> false

newLabel :: LowerM CodeLabel
newLabel = do
  s0@{ next } <- get
  put (s0 { next = next + 1 })
  pure $ CodeLabel next

-- mkBranch :: _ 
-- mkBranch = case _ of 
--   c@(KReturn : _) -> c 

compileModule :: IR.Module -> LowerM Program
compileModule (IR.Module m) = tailRecM go (m.decls /\ empty (unwrap m.name))
  where
  go (decls /\ program@(Program p)) = case Array.uncons decls of
    Nothing -> pure $ Done program
    Just { head, tail: rest } -> case head of
      IR.NonRec (IR.Decl { ident, term }) -> do
        { init, text } <- compileDecl { ident, term }
        pure $ Loop $
          Tuple
            rest
            ( Program $ p
                { text = if codeLength text > 0 then Array.snoc p.text text else p.text
                , init = p.init <> init
                , syms = case ident of 
                    Ident "it" -> p.syms 
                    _ -> p.syms `Array.snoc` ident 
                }
            )
      IR.Rec delcs' -> unsafeCrashWith "Not implemented!"

compileDecl :: { ident :: Ident, term :: ELTerm } -> LowerM { init :: Array Instruction, text :: CodeSection }
compileDecl { ident, term } = do
  resetToCompile
  resetLabel
  { init: _, text: _ }
    <$> compileInitializerSection
    <*> compileTextSection
  where
  compileInitializerSection = do
    code <- compileTerm Nothing term (contOfIdent ident)
    pure $ List.foldl Array.snoc [] code

  compileTextSection = do
    code <- compileMore List.Nil
    pure $
      CodeSection
        { lbl: unwrap ident
        , code: List.foldl Array.snoc [] code
        }

  contOfIdent :: Ident -> List Instruction
  contOfIdent = case _ of
    Ident "it" -> KStop : List.Nil
    _ -> KSetGlobal ident : List.Nil

  compileMore :: List Instruction -> LowerM (List Instruction)
  compileMore k = do
    getToCompile >>= case _ of
      Just cb
        | 1 <- cb.arity -> do
            code <- compileTerm Nothing cb.term (KReturn : k)
            pure $ KLabel (Just $ unwrap ident) cb.lbl : KStartFun : code
        | otherwise -> do
            code <- compileTerm Nothing cb.term (KReturn : k)
            pure $ KLabel (Just $ unwrap ident) cb.lbl : KStartFun :
              nGrab (cb.arity - 1) code
      Nothing -> pure k

  getToCompile :: LowerM (Maybe { lbl :: CodeLabel, arity :: Int, term :: ELTerm })
  getToCompile = do
    s0@{ more } <- get
    for (List.uncons more) \{ head: cb, tail: rest } -> do
      put (s0 { more = rest })
      pure cb

  resetToCompile :: LowerM Unit
  resetToCompile = modify_ (\s0 -> s0 { more = List.Nil })

  resetLabel :: LowerM Unit
  resetLabel = modify_ \s0 -> s0 { next = 0 }

addToCompile :: CodeLabel -> Int -> ELTerm -> LowerM Unit
addToCompile lbl arity term = do
  s@{ more } <- get
  put (s { more = { lbl, arity, term } : more })

nGrab :: Int -> List Instruction -> List Instruction
nGrab n = go n
  where
  go 0 code = code
  go n' code = go (n' - 1) (KGrab : code)

compileTerm :: Maybe CodeLabel -> ELTerm -> List Instruction -> LowerM (List Instruction)
compileTerm _ = go
  where
  go term k = case term of
    ELVar _ (Var n) -> pure $ KAccess n : k
    ELConst _ cst -> pure $ KQuote cst : k
    ELApp _ tmAbs args
      | KReturn : k' <- k -> do
          body <- go tmAbs (KTailApply : k')
          compArgList args (KPush : body)
      | otherwise -> do
          body <- go tmAbs (KApply : k)
          (KPushMark : _) <$>
            compArgList args (KPush : body)
    ELAbs _ arity tmBody
      | isTail k -> do
          nGrab arity <$> go tmBody k
      | otherwise -> do
          lbl <- newLabel
          addToCompile lbl arity tmBody
          pure $ KClosure lbl : k
    ELPrim _ prim args -> case prim of
      PGetGlobal id -> pure $ KGetGlobal id : k
      _ -> compArgList args (compPrim prim k)
    _ -> unsafeCrashWith "Not Implemented"

  compPrim prim k = case prim of
    PAccess n -> KAccess n : k
    P_i32_add -> K_i32_add : k
    P_i32_sub -> K_i32_sub : k
    P_i32_mul -> K_i32_mul : k
    P_i32_div -> K_i32_div : k
    P_i32_mod -> K_i32_mod : k
    P_i32_equ -> K_i32_equ : k
    P_i32_neq -> K_i32_neq : k
    P_i32_le -> K_i32_le : k
    P_i32_lt -> K_i32_lt : k
    P_i32_ge -> K_i32_lt : K_log_not : k 
    P_i32_gt -> K_i32_le : K_log_not : k
    P_log_and -> K_log_and : k
    P_log_or -> K_log_or : k
    P_log_not -> K_log_not : k
    P_log_xor -> K_log_xor : k
    _ -> unsafeCrashWith "Impoissible"

  compArgList terms k = case Array.uncons terms of
    Nothing -> pure k
    Just { head: term, tail: rest }
      | [] <- rest -> go term k
      | otherwise -> do
          code <- go term k
          compArgList rest (KPush : code)