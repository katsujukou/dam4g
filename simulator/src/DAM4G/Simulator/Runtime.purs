module DAM4G.Simulator.Runtime where

import Prelude

import DAM4G.Simulator.Instruction (Constant(..), Instruction(..), ConstructorTag)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List, (!!))
import Data.List as L
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data Value
  = Imd Int
  | Ptr Addr
  | Epsiron
  | Uninitialized

derive instance Eq Value
derive instance Generic Value _
instance Show Value where
  show = genericShow

type Addr = Int

type CPU =
  { pgc :: Addr
  , acc :: Value
  , arg :: List Value
  , env :: List Value
  , ret :: List { pgc :: Addr, env :: List Value }
  }

-- Entity which should be allocated in the heap.
data Obj
  = Closure Addr (Array Value)
  | Cons Value Addr

derive instance Eq Obj

derive instance Generic Obj _
instance Show Obj where
  show = genericShow

data ObjTag
  = TClos
  | TBlk Int

derive instance Eq ObjTag
derive instance Generic ObjTag _
instance Show ObjTag where
  show = genericShow

valueOfTag :: ObjTag -> Int
valueOfTag = case _ of
  TClos -> 99
  TBlk t -> t

data ObjElement
  = Tag ObjTag Int
  | Component Value

derive instance Eq ObjElement
derive instance Generic ObjElement _
instance Show ObjElement where
  show = genericShow

type Heap = Array (Maybe ObjElement)

type Runtime =
  { cpu :: CPU
  , heap :: Heap
  , globals :: Map String Value
  , state :: RuntimeState
  }

maxSizeOfEnv :: Int
maxSizeOfEnv = 128

initialState :: Runtime
initialState =
  { cpu:
      { pgc: 0
      , acc: Uninitialized
      , arg: L.Nil
      , env: L.Nil
      , ret: L.Nil
      }
  , heap: ((0 .. 511) <#> const Nothing)
  , globals: Map.empty
  , state: PowerOff
  }

data RuntimeState = PowerOff | Loaded | Running | Pause | Halt | Glitch

derive instance Eq RuntimeState
derive instance Generic RuntimeState _
instance Show RuntimeState where
  show = genericShow

data RuntimeError
  = EvalStuck
  | ProgramOutOfRange
  | UndefinedName String
  | MemoryExhausted
  | OutOfMemory
  | StackOutOfRange
  | NullPointer
  | AccessToUninitializedMemory
  | NotAPointer
  | NotAConsCell
  | NotANumericValue
  | NotABlockTag
  | NotABlockComponent
  | IllegalPointerAccess
  | BrokenBlockValue
  | CallToNotAFunction
  | EmptyArgumentStack
  | PopEmptyReturnStack
  | TypeError String

describe :: RuntimeError -> String
describe = case _ of
  EvalStuck -> "Evaluation stuck."
  ProgramOutOfRange -> "Program Out of Range."
  UndefinedName name -> "Undefined name: " <> name <> "."
  MemoryExhausted -> "Memory exhausted."
  OutOfMemory -> "Out of Memory"
  NullPointer -> "Null Pointer"
  EmptyArgumentStack -> "Empty argument stack."
  AccessToUninitializedMemory -> "Access to uninitialzed memory"
  StackOutOfRange -> "Stack Out of Range."
  NotAPointer -> "Not a pointer."
  NotAConsCell -> "Not a cons cell"
  NotABlockTag -> "Not a block tag"
  BrokenBlockValue -> "Broken block value"
  TypeError ty -> "TypeError: Expecting " <> ty
  NotANumericValue -> "Not a numeric value."
  NotABlockComponent -> "Not a block component."
  IllegalPointerAccess -> "Illegal pointer access"
  CallToNotAFunction -> "Call to not a function"
  PopEmptyReturnStack -> "Pop empty return stack"

type Machine m =
  { --allocator :: Obj -> m Addr
    -- , deref :: Addr -> m (Maybe Obj),
    loadAccum :: Value -> m Unit
  , readAccum :: m Value
  , pushArg :: Value -> m Unit
  , pushEnv :: Value -> RuntimeM m Unit
  , popArg :: m (Either RuntimeError Value)
  , getEnv :: m (List Value)
  , setEnv :: (List Value) -> m Unit
  -- , current :: Addr -> m Obj
  , jumpTo :: Addr -> m RuntimeState
  , alloc :: Int -> RuntimeM m Addr
  , write :: Addr -> ObjElement -> RuntimeM m Unit
  , dereference :: Addr -> m (Either RuntimeError ObjElement)
  , lookupGlobal :: String -> m (Maybe Value)
  , updateGlobal :: String -> Value -> m Unit
  , popReturn :: m (Either RuntimeError { pgc :: Addr, env :: List Value })
  , pushReturn :: RuntimeM m Unit
  , progress :: m RuntimeState
  , readProgramCounter :: RuntimeM m Addr
  , stop :: m RuntimeState
  , error :: RuntimeError -> m RuntimeState
  }

type RuntimeM :: (Type -> Type) -> Type -> Type
type RuntimeM m a = m (Either RuntimeError a)

step :: forall m. Monad m => Machine m -> Instruction -> m RuntimeState
step m = case _ of
  KNoop -> m.progress
  KStop -> m.stop

  KQuote imm
    | CstInt i <- imm -> m.loadAccum (Imd i) *> m.progress
    | CstBool b <- imm -> m.loadAccum (Imd $ if b then 1 else 0) *> m.progress

  KGetGlobal (Right name) -> do
    m.lookupGlobal name >>= case _ of
      Just v -> m.loadAccum v *> m.progress
      Nothing -> m.error $ UndefinedName name

  KSetGlobal (Right sym) -> do
    v <- m.readAccum
    m.updateGlobal sym v *> m.progress

  KField n -> do
    m.readAccum >>= case _ of
      Ptr ptr -> do
        m.dereference ptr >>= case _ of
          Left err -> m.error err
          Right c
            | Tag (TBlk _) sz <- c ->
                if n >= sz then m.error IllegalPointerAccess
                else do
                  m.dereference (ptr + 1 + n) >>= case _ of
                    Left err -> m.error err
                    Right el
                      | Component v <- el -> m.loadAccum v *> m.progress
                      | otherwise -> m.error NotABlockComponent

            | otherwise -> m.error NotABlockComponent
      _ -> m.error $ NotAPointer

  KMakeBlock tag sz -> do
    m.alloc (sz + 1) >>= case _ of
      Left err -> m.error err
      Right ptr -> do
        buildBlock ptr tag sz >>= case _ of
          Left err -> m.error err
          Right ptr' -> m.loadAccum (Ptr ptr') *> m.progress

  KClosure (Right fptr) -> do
    env <- m.getEnv
    buildClosure fptr env >>= case _ of
      Left err -> m.error err
      Right ptr -> m.loadAccum (Ptr ptr) *> m.progress

  KApply -> do
    arg <- m.popArg
    case arg of
      Left err -> m.error err
      Right v -> do
        m.readAccum >>= case _ of
          Ptr p -> do
            readClosure p >>= case _ of
              Left err -> m.error err
              Right { fptr, env } -> do
                m.pushReturn >>= case _ of
                  Left err -> m.error err
                  Right _ -> do
                    m.setEnv (List.Cons v env) *> m.jumpTo fptr
          _ -> m.error NotAPointer
  KPush -> (m.readAccum >>= m.pushArg) *> m.progress

  KPushMark -> m.pushArg Epsiron *> m.progress

  KGrab -> do
    m.popArg >>= case _ of
      Left err -> m.error err
      Right val
        | Epsiron <- val -> do
            m.popReturn >>= case _ of
              Left err -> m.error err
              Right { pgc: c1, env: e1 } -> do
                e0 <- m.getEnv
                m.progress *> m.readProgramCounter >>= case _ of
                  Left err -> m.error err
                  Right c0 -> do
                    buildClosure c0 e0 >>= case _ of
                      Left err -> m.error err
                      Right addr -> do
                        m.loadAccum (Ptr addr)
                        m.setEnv e1
                        m.jumpTo c1
        | otherwise -> m.pushEnv val *> m.progress

  KReturn -> do
    m.popArg >>= case _ of
      Left err -> m.error err
      Right val
        | Epsiron <- val -> do
            m.popReturn >>= case _ of
              Left err -> m.error err
              Right { pgc, env } -> do
                m.setEnv env *> m.jumpTo pgc
        | otherwise -> do
            m.readAccum >>= case _ of
              Ptr p -> do
                readClosure p >>= case _ of
                  Left err -> m.error err
                  Right { fptr, env: e1 } -> do
                    m.setEnv (val `L.Cons` e1)
                      *> m.jumpTo fptr
              _ -> m.error $ NotAPointer

  -- KTailApply -> do
  --   acc <- m.readAccum
  --   m.popArg >>= case acc, _ of
  --     -- _, Left err -> m.error err
  --     -- Ptr p, Right arg -> do
  --     --   derefClosure p >>= case _ of
  --     --     Left err -> m.error err
  --     --     Right (Closure fptr env) -> do
  --     --       m.setEnv env
  --     --       pushEnv arg >>= case _ of
  --     --         Left err -> m.error err
  --     --         Right _ -> m.jumpTo fptr
  --     _, _ -> m.error CallToNotAFunction

  KLet -> do
    v <- m.readAccum
    m.pushEnv v >>= case _ of
      Right _ -> m.progress
      Left err -> m.error err

  KEndLet n -> do
    -- TODO envの長さよりnが大きいとエラーにしたほうがよさそう
    e <- m.getEnv
    m.setEnv (L.drop n e) *> m.progress
  -- KDummies _ -> unsafeCrashWith "Not Implemented: Dummies"
  -- KUpdate _ -> unsafeCrashWith "Not Implemented: Uodate"

  K_i32_add -> evalArithmetic (+)
  K_i32_sub -> evalArithmetic (-)
  K_i32_mul -> evalArithmetic (*)
  K_i32_div -> evalArithmetic (/)
  K_i32_mod -> evalArithmetic mod
  K_i32_equ -> evalTest (==)
  K_i32_neq -> evalTest (/=)
  K_i32_le -> evalTest (<=)
  K_i32_lt -> evalTest (<)

  KAccess n -> do
    env <- m.getEnv
    case env !! n of
      Nothing -> m.error StackOutOfRange
      Just v -> m.loadAccum v *> m.progress

  -- KBranch ofs -> m.jumpTo ofs
  -- KBranchIf ofs -> do
  --   v <- m.readAccum
  --   case v of
  --     Imd 0 -> m.progress
  --     Imd _ -> m.jumpTo ofs
  --     _ -> m.error NotANumericValue
  -- KBranchIfNot ofs -> do
  --   v <- m.readAccum
  --   case v of
  --     Imd 0 -> m.jumpTo ofs
  --     Imd _ -> m.progress
  --     _ -> m.error NotANumericValue
  KBranchIfNotImm cst (Right ofs) -> do
    a <- m.readAccum
    case a, cst of
      Imd n1, CstInt n2 ->
        if n1 == n2 then m.progress
        else m.jumpTo ofs
      Imd n1, CstBool b ->
        if not b && n1 == 0 then m.progress
        else m.jumpTo ofs
      _, _ -> m.error NotANumericValue

  KBranchIfNotTag tag (Right ofs) -> do
    mc <- dereferenceAccumTag
    case mc of
      Left err -> m.error err
      Right { tag: t } ->
        if t == tag then m.progress
        else m.jumpTo ofs

  KBranchIfEqTag tag (Right ofs) -> do
    mc <- dereferenceAccumTag
    case mc of
      Left err -> m.error err
      Right { tag: t } ->
        if t /= tag then m.progress
        else m.jumpTo ofs

  KExit -> m.error EvalStuck

  _ -> m.error EvalStuck
  where
  buildBlock :: Addr -> ConstructorTag -> Int -> RuntimeM m _
  buildBlock ptr tag sz = do
    let
      popArgs args 0 = pure $ Right $ Tag (TBlk tag) sz `Array.cons` args
      popArgs args n = do
        (if n == 1 then Right <$> m.readAccum else m.popArg) >>= case _ of
          Left err -> pure $ Left err
          Right v -> popArgs (Array.cons (Component v) args) (n - 1)
    popArgs [] sz >>= case _ of
      Left err -> pure $ Left err
      Right blk -> do
        writeHeap ptr blk

  buildClosure :: Addr -> List Value -> RuntimeM m _
  buildClosure fptr env = do
    let
      sz = List.length env
      closure = Array.fold
        [ [ Tag TClos sz, Component (Ptr fptr) ]
        , Component <$> Array.fromFoldable env
        ]

    m.alloc (2 + sz) >>= case _ of
      Left err -> pure $ Left err
      Right ptr -> writeHeap ptr closure

  readClosure :: Addr -> RuntimeM m { fptr :: Addr, env :: List Value }
  readClosure ptr = do
    m.dereference ptr >>= case _ of
      Left err -> pure $ Left err
      Right elem
        | Tag TClos sz <- elem -> do
            m.dereference (ptr + 1) >>= case _ of
              Left err -> pure $ Left err
              Right elem2
                | Component (Ptr fptr) <- elem2 -> do
                    if sz == 0 then pure $ Right { fptr, env: L.Nil }
                    else do
                      readHeapBlock (ptr + 1) sz >>= case _ of
                        Left err -> pure $ Left err
                        Right env -> pure $ Right { fptr, env: L.fromFoldable env }
                | otherwise -> pure $ Left BrokenBlockValue
        | otherwise -> pure $ Left $ TypeError "Closure"
  writeHeap ptr block = do
    let
      go p = Array.uncons >>> case _ of
        Nothing -> pure $ Right ptr
        Just { head, tail } -> do
          m.write p head >>= case _ of
            Left err -> pure $ Left err
            Right _ -> go (p + 1) tail
    go ptr block

  readHeapBlock ptr n = do
    let
      go 0 block = pure $ Right block
      go n' block = do
        m.dereference (ptr + n') >>= case _ of
          Left err -> pure $ Left err
          Right elem
            | Component v <- elem -> go (n' - 1) (Array.snoc block v)
            | otherwise -> pure $ Left NotABlockComponent
    go n []

  dereferenceAccumTag = do
    m.readAccum >>= case _ of
      Ptr p -> do
        m.dereference p >>= case _ of
          Left err -> pure $ Left err
          Right mc
            | Tag tag sz <- mc -> pure $ Right { tag: valueOfTag tag, sz }
            | otherwise -> pure $ Left $ NotABlockTag
      _ -> pure $ Left $ NotAPointer
  -- accessEnv :: Int -> _ (Either RuntimeError Value)
  -- accessEnv n = unsafeCrashWith "Not implemented"

  -- uncons :: Int -> Addr -> m (Either RuntimeError Value)
  -- uncons ofs addr = do
  --   m.dereference addr >>= case _ of
  --     Left err -> pure $ Left err
  --     Right elem
  --       | Tag TCons _ <- elem -> do
  --           m.dereference (addr + ofs) >>= case _ of
  --             Left err -> pure $ Left err
  --             Right elem
  --               | Component v <- elem -> pure $ Right v
  --               | otherwise -> pure $ Left $ NotABlockComponent
  --       | otherwise -> pure $ Left IllegalPointerAccess

  -- car :: Addr -> m (Either RuntimeError Value)
  -- car = uncons 1

  -- cdr :: Addr -> m (Either RuntimeError Addr)
  -- cdr = uncons 2 >=> case _ of
  --   Left err -> pure $ Left err
  --   Right v
  --     | Ptr addr <- v -> pure $ Right addr
  --     | otherwise -> pure $ Left NotAPointer

  -- -- derefClosure addr = do
  -- --   m.dereference >>= case _ of 
  -- --     Left err -> pure $ Left err
  -- --     Right el 
  -- --       | Tag   

  -- pushEnv val = do
  --   env1 <- m.getEnv
  --   allocate (Cons val env1) >>= case _ of
  --     Left err -> pure $ Left err
  --     Right addr -> m.setEnv addr $> Right unit

  -- sizeOf :: Obj -> m (Either RuntimeError Int)
  -- sizeOf = case _ of
  --   _ -> pure $ Right 0
  -- -- Reserved -> pure $ Right 1
  -- -- Cons _ _ -> pure $ Right 3
  -- -- Closure _ e -> rmap (_ + 2) <$> sizeOfEnv e
  -- -- where
  -- -- sizeOfEnv :: Addr -> m (Either RuntimeError Int)
  -- -- sizeOfEnv env = do
  -- --   let
  -- --     go n 0 = pure $ Right n
  -- --     go n cdr = do
  -- --       m.dereference cdr >>= case _ of
  -- --         Left err -> pure $ Left err
  -- --         Right obj
  -- --           | Cons _ cdr <- obj -> go (n + 1) cdr
  -- --           | otherwise -> pure $ Left NotAConsCell
  -- --   go 0 env

  -- allocate obj = do
  --   sizeOf obj >>= case _ of
  --     Left err -> pure $ Left err
  --     Right sz -> do
  --       ptr <- m.alloc sz
  --       memcpy ptr obj

  -- memcpy ptr _ = --case _ of 

  --   -- Cons car cdr -> do
  --   -- for [Tag TCons 2, car, Ptr cdr]
  --   pure $ Right 0

  -- consumeEnv :: Int -> RuntimeM m Addr
  -- consumeEnv n = do
  --   let
  --     go :: Int -> Addr -> m (Either RuntimeError Addr)
  --     go 0 addr = pure $ Right addr
  --     go n' addr = do
  --       cdr addr >>= case _ of
  --         Left err -> pure $ Left err
  --         Right addr' -> go (n' - 1) addr'
  --   m.getEnv >>= go n

  evalArithmetic op = do
    a <- m.readAccum
    m.popArg >>= case a, _ of
      _, Left err -> m.error err
      Imd i1, Right (Imd i2) -> do
        m.loadAccum (Imd $ op i1 i2) *> m.progress
      _, _ -> m.error NotANumericValue

  evalTest op = do
    a <- m.readAccum
    m.popArg >>= case a, _ of
      _, Left err -> m.error err
      Imd i1, Right (Imd i2) -> do
        if op i1 i2 then do
          m.loadAccum (Imd 1) *> m.progress
        else do
          m.loadAccum (Imd 0) *> m.progress
      _, _ -> m.error NotANumericValue

