module DAM4G.Simulator.Hooks.UseStore where

import Prelude

import Control.Monad.Rec.Class (forever)
import DAM4G.Simulator.Compiler (Compiler, GdoFile, emptyGdoFile, loadCompiler, loadGdoFile)
import DAM4G.Simulator.Compiler as Compiler
import DAM4G.Simulator.Instruction (Instruction(..))
import DAM4G.Simulator.Runtime (Addr, Runtime, RuntimeError(..), RuntimeState(..), Value)
import DAM4G.Simulator.Runtime as Runtime
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref as Ref
import Halogen.Helix (UseHelix, UseHelixHook, makeStore')
import Halogen.Hooks (class HookNewtype, type (<>), HookType, UseEffect, UseRef, UseState, useLifecycleEffect, useRef, useState)
import Halogen.Hooks as Hooks
import Halogen.Query.HalogenM (ForkId(..))
import Partial.Unsafe (unsafeCrashWith)

data AppState
  = Start
  | Loading
  | Ready

derive instance Eq AppState
derive instance Generic AppState _
instance Show AppState where
  show = genericShow

type Program =
  { offset :: Int
  , code ::
      Array
        { label :: Maybe String
        , opcode :: Int
        }
  }

type State =
  { appstate :: AppState
  , gasm :: GdoFile
  , compileError :: Maybe String
  , program ::
      { code :: Array (Int /\ Instruction /\ Array Int)
      , entrypoint :: Int
      }
  , runtime :: Runtime
  , breakpoints :: Map.Map Int Boolean
  }

data Action
  = SetLoading
  | SetReady
  | SetGasm GdoFile
  | SetProgram Int (Array (Int /\ Instruction /\ Array Int))
  | SetCompileError String
  | ResetCompile
  | SetProgramCounter Int
  | SetGlobals (Map.Map String Value)
  | SetHeap Runtime.Heap
  | LoadAccum Runtime.Value
  | SetEnv (List.List Value)
  | SetArgStack (List.List Value)
  | PushReturnStack Addr (List.List Value)
  | SetReturnStack (List.List { pgc :: Addr, env :: List.List Value })
  | SetRuntimeState RuntimeState
  | PushArg Runtime.Value
  | SetBreakPoint Int Boolean

newtype ExecutionId = ExecutionId ForkId

useStore :: forall m a. Eq a => MonadEffect m => UseHelixHook State Action a m
useStore = makeStore' "store" reducer initialState
  where
  initialState =
    { appstate: Start
    , gasm: emptyGdoFile
    , compileError: Nothing
    , program: { entrypoint: 0, code: [] }
    , runtime: Runtime.initialState
    , breakpoints: Map.empty
    }
  reducer st = case _ of
    SetLoading -> st { appstate = Loading }
    SetReady -> st { appstate = Ready }
    SetProgram entrypoint code -> st
      { program = { entrypoint, code }
      , runtime = st.runtime { state = Loaded }
      }
    SetCompileError error -> st { compileError = Just error }
    SetGasm gdofile -> st { gasm = gdofile }
    ResetCompile -> st { compileError = Nothing, gasm = emptyGdoFile }
    SetRuntimeState state -> st
      { runtime = st.runtime { state = state }
      }
    SetProgramCounter addr -> do
      let cpu' = st.runtime.cpu { pgc = addr }
      st { runtime = st.runtime { cpu = cpu' } }
    LoadAccum val -> do
      let cpu' = st.runtime.cpu { acc = val }
      st { runtime = st.runtime { cpu = cpu' } }
    SetArgStack arg -> do
      let cpu' = st.runtime.cpu { arg = arg }
      st { runtime = st.runtime { cpu = cpu' } }
    SetGlobals globals -> do
      st { runtime = st.runtime { globals = globals } }
    SetHeap heap -> st { runtime = st.runtime { heap = heap } }
    PushReturnStack pgc env -> do
      let cpu' = st.runtime.cpu { ret = List.Cons { env, pgc } st.runtime.cpu.ret }
      st { runtime = st.runtime { cpu = cpu' } }
    SetReturnStack ret -> do
      let cpu' = st.runtime.cpu { ret = ret }
      st { runtime = st.runtime { cpu = cpu' } }
    PushArg val -> do
      let cpu' = st.runtime.cpu { arg = List.Cons val st.runtime.cpu.arg }
      st { runtime = st.runtime { cpu = cpu' } }
    SetEnv env -> do
      let cpu' = st.runtime.cpu { env = env }
      st { runtime = st.runtime { cpu = cpu' } }
    SetBreakPoint addr enabled -> do
      st { breakpoints = Map.insert addr enabled st.breakpoints }

type AppInterface m =
  { gasm :: GdoFile
  , runCompile :: String -> Hooks.HookM m Unit
  , getCompileError :: Hooks.HookM m (Maybe String)
  , getGasm :: Hooks.HookM m GdoFile
  , labeledCode :: LabeledCode
  , getLabeledCode :: Hooks.HookM m LabeledCode
  , program ::
      { entrypoint :: Int
      , code :: Array (Int /\ Instruction /\ Array Int)
      }
  , runtime :: Runtime.Runtime
  , getRuntime :: Hooks.HookM m Runtime.Runtime
  , pushArg :: Runtime.Value -> Hooks.HookM m Unit
  , popArg :: Hooks.HookM m (Either String Value)
  , readAccum :: Hooks.HookM m Value
  , runProgram :: Hooks.HookM m Unit
  , stepProgram :: Hooks.HookM m Unit
  , breakpoints :: Map.Map Int Boolean
  , listBreakpoints :: Hooks.HookM m (Map.Map Int Boolean)
  , addBreakpoint :: Int -> Hooks.HookM m Unit
  , toggleBreakpoint :: Int -> Hooks.HookM m Boolean
  }

foreign import data UseApp :: HookType

type LabeledCode =
  { text :: Array (Maybe String /\ Instruction)
  , entry :: Array (Maybe String /\ Instruction)
  }

type UseApp' = UseHelix State
  <> UseState LabeledCode
  <> UseRef (Maybe Compiler)
  <> UseEffect
  <> Hooks.Pure

instance HookNewtype UseApp UseApp'

useApp :: forall m. MonadAff m => Hooks.Hook m UseApp (AppInterface m)
useApp = Hooks.wrap hook
  where
  hook :: Hooks.Hook _ UseApp' _
  hook = Hooks.do
    state /\ ctx <- useStore identity
    labeledCode /\ labeldedCodeId <- useState
      { entry: []
      , text: []
      }
    _ /\ compilerRef <- useRef Nothing

    let
      runCompile :: String -> Hooks.HookM m Unit
      runCompile src = do
        ctx.dispatch ResetCompile
        comp <- liftEffect $ Ref.read compilerRef
        case comp of
          Nothing -> liftEffect $ throw "Compiler is not loaded"
          Just compiler -> do
            liftEffect (Compiler.compile compiler src) >>= case _ of
              Left err -> ctx.dispatch $ SetCompileError err
              Right { emitBin } -> do
                gasm <- liftEffect (emitBin >>= loadGdoFile)
                ctx.dispatch $ SetGasm gasm
                loadProgram ctx gasm

      fetchCode = do
        { runtime, program } <- ctx.getState
        case program.code !! runtime.cpu.pgc of
          Just (_ /\ instr /\ _) -> pure instr
          Nothing -> unsafeCrashWith "Oops"

      step :: Runtime.Runtime -> Instruction -> Hooks.HookM m Runtime.RuntimeState
      step = do
        let
          machine :: Runtime.Machine (Hooks.HookM m)
          machine = do
            let
              progress = do
                { runtime, program } <- ctx.getState
                if runtime.cpu.pgc < Array.length program.code - 1 then do
                  ctx.dispatch $ SetProgramCounter (runtime.cpu.pgc + 1)
                  pure Runtime.Running
                else do
                  Console.logShow runtime
                  unsafeCrashWith "Eval stuck"

              error err = do
                Console.log ("Oops!" <> Runtime.describe err)
                pure Runtime.Glitch

              stop = do
                pure Runtime.Halt

              lookupGlobal ident = do
                { runtime } <- ctx.getState
                pure $ Map.lookup ident runtime.globals

              updateGlobal ident v = do
                { runtime } <- ctx.getState
                let
                  newGlobals =
                    case Map.lookup ident runtime.globals of
                      Just _ -> Map.update (const (Just v)) ident runtime.globals
                      Nothing -> Map.insert ident v runtime.globals
                ctx.dispatch $ SetGlobals newGlobals

              alloc sz = do
                { runtime } <- ctx.getState
                let
                  search ofs heap =
                    case Array.uncons heap of
                      Nothing -> Left MemoryExhausted
                      Just { tail }
                        | region <- Array.take sz tail
                        , Array.length region == sz
                        , Array.all isNothing region -> Right ofs
                        | otherwise -> search (ofs + 1) tail
                pure $ search 1 runtime.heap

              write ptr v = do
                { runtime } <- ctx.getState
                case Array.updateAt ptr (Just v) runtime.heap of
                  Just heap' -> Right <$> ctx.dispatch (SetHeap heap')
                  _ -> pure $ Left OutOfMemory

              dereference 0 = pure $ Left Runtime.NullPointer
              dereference addr = do
                { runtime } <- ctx.getState
                case runtime.heap !! addr of
                  Just (Just obj) -> pure $ Right obj
                  Just Nothing -> do
                    Console.logShow runtime.heap
                    pure $ Left Runtime.AccessToUninitializedMemory
                  Nothing -> pure $ Left $ Runtime.OutOfMemory

              -- getEnv = do
              --   { runtime } <- ctx.getState
              --   pure runtime.cpu.env
              pushEnv v = do
                { runtime } <- ctx.getState
                if List.length runtime.cpu.env > Runtime.maxSizeOfEnv then
                  pure $ Left OutOfMemory
                else do
                  Right <$> ctx.dispatch (SetEnv (v : runtime.cpu.env))

              -- current env = do
              --   { runtime: { cpu: { pgc } }, program: { code } } <- ctx.getState
              --   case code !! pgc of
              --     Just (ofs /\ _) -> pure $ Runtime.Closure ofs env
              --     _ -> unsafeCrashWith "Impossible"

              pushArg val = do
                ctx.dispatch $ PushArg val

              popArg = do
                { runtime: { cpu } } <- ctx.getState
                case cpu.arg of
                  List.Cons val args -> do
                    ctx.dispatch (SetArgStack args) $> Right val
                  _ -> pure $ Left Runtime.EmptyArgumentStack

              popReturn = do
                { runtime } <- ctx.getState
                case runtime.cpu.ret of
                  List.Cons { pgc, env } tail -> do
                    ctx.dispatch $ SetReturnStack tail
                    pure $ Right { pgc, env }
                  List.Nil -> pure $ Left Runtime.PopEmptyReturnStack

              pushReturn = do
                { program, runtime } <- ctx.getState
                case program.code !! (runtime.cpu.pgc + 1) of
                  Nothing -> pure $ Left $ Runtime.ProgramOutOfRange
                  Just (ofs /\ _) -> do
                    ctx.dispatch $ PushReturnStack ofs runtime.cpu.env
                    pure $ Right unit

              jumpTo targetOfs = do
                Console.log $ "targetOfs:" <> show targetOfs
                { program: { code }, runtime } <- ctx.getState
                case Array.findIndex (fst >>> (_ == targetOfs)) code of
                  Nothing -> unsafeCrashWith "Oooooooops!"
                  Just ofs -> ctx.dispatch (SetProgramCounter ofs)
                    *> pure Runtime.Running

              readProgramCounter = do
                { program, runtime } <- ctx.getState
                case program.code !! runtime.cpu.pgc of
                  Nothing -> pure $ Left Runtime.ProgramOutOfRange
                  Just (ofs /\ _) -> pure $ Right ofs

            { loadAccum: LoadAccum >>> ctx.dispatch
            , readAccum: ctx.getState <#> \st -> st.runtime.cpu.acc
            , progress
            , readProgramCounter
            , jumpTo
            , pushArg
            , popArg
            , popReturn
            , pushReturn
            -- , current
            -- , allocate
            , dereference
            , getEnv: ctx.getState <#> _.runtime >>> _.cpu >>> _.env
            , pushEnv
            , alloc
            , write
            , setEnv: ctx.dispatch <<< SetEnv
            , lookupGlobal
            , updateGlobal
            , error
            , stop
            }
        Runtime.step machine

      execInstrucion = do
        { runtime } <- ctx.getState
        step runtime =<< fetchCode

      stepProgram = do
        -- liftAff $ Aff.delay $ Milliseconds 300.0
        { program, runtime, breakpoints } <- ctx.getState
        case runtime.state of
          Runtime.Running -> do
            case program.code !! runtime.cpu.pgc of
              Just (ofs /\ _)
                | Just true <- Map.lookup ofs breakpoints -> do
                    Console.log $ "Break at " <> show ofs
                    ctx.dispatch $ SetRuntimeState Pause
              _ -> execStep
          Runtime.Pause -> do
            nextState <- execInstrucion >>= case _ of
              Runtime.Running -> pure Runtime.Pause
              st -> pure st
            ctx.dispatch $ SetRuntimeState nextState
          _ -> pure unit
        where
        execStep = do
          nextState <- execInstrucion
          case nextState of
            Runtime.Running -> stepProgram
            _ -> pure unit

      runProgram = do
        { program } <- ctx.getState
        let entrypointOfs = program.entrypoint
        case program.code # Array.findIndex (fst >>> (_ == entrypointOfs)) of
          Just entrypointIdx -> do
            ctx.dispatch $ SetProgramCounter entrypointIdx
            ctx.dispatch $ SetRuntimeState Running
            stepProgram
          Nothing -> do
            unsafeCrashWith "Oops"

      listBreakpoints = ctx.getState <#> _.breakpoints

      addBreakpoint addr = do
        Console.log $ "Add break point: " <> show addr
        ctx.dispatch $ SetBreakPoint addr true

      toggleBreakpoint addr = do
        { breakpoints } <- ctx.getState
        case Map.lookup addr breakpoints of
          Nothing -> liftEffect $ throw "Breakpoint not set"
          Just enabled -> do
            ctx.dispatch $ SetBreakPoint addr (not enabled)
            pure $ not enabled

    useLifecycleEffect do
      ctx.dispatch SetLoading
      void $ Hooks.fork do
        compiler <- liftAff loadCompiler
        liftEffect $ Ref.write (Just compiler) compilerRef
        runCompile ""

      pure Nothing

    Hooks.pure
      { gasm: state.gasm
      , getCompileError: ctx.getState <#> _.compileError
      , getGasm: ctx.getState <#> _.gasm
      , runCompile
      , labeledCode
      , getLabeledCode: Hooks.get labeldedCodeId
      , program: state.program
      , runtime: state.runtime
      , getRuntime: ctx.getState <#> _.runtime
      , pushArg: PushArg >>> ctx.dispatch
      , popArg: ctx.getState >>= \{ runtime: { cpu } } -> do
          case cpu.arg of
            List.Nil -> pure $ Left "Empty argument stack"
            List.Cons hd _ -> pure $ Right hd
      , readAccum: ctx.getState >>= \st -> pure st.runtime.cpu.acc
      , runProgram
      , stepProgram
      , breakpoints: state.breakpoints
      , listBreakpoints
      , addBreakpoint
      , toggleBreakpoint
      }

  loadProgram ctx gdofile = do
    let
      resolveCodeLabelAndName =
        \{ ofs, instr, opcode } -> case instr of
          KClosure tgt
            | Left cl <- tgt
            , Just addr <- Map.lookup cl gdofile.lbls -> ofs /\ KClosure (Right addr) /\ opcode
          KGetGlobal sym
            | Left symOfs <- sym
            , Just name <- gdofile.syms !! symOfs -> ofs /\ KGetGlobal (Right name) /\ opcode
          KSetGlobal sym
            | Left symOfs <- sym
            , Just name <- gdofile.syms !! symOfs -> ofs /\ KSetGlobal (Right name) /\ opcode
            | otherwise -> unsafeCrashWith $ "Whoa!?" <> show gdofile.syms <> show sym
          _ -> ofs /\ instr /\ opcode
      entry = resolveCodeLabelAndName <$> gdofile.entry
      text = resolveCodeLabelAndName <$> gdofile.text
      program = text <> entry
    entrypoint <- case entry !! 0 of
      Just (ofs /\ _) -> Console.log (show $ entry !! 0) $> ofs
      Nothing -> unsafeCrashWith "No entry point!"
    ctx.dispatch $ SetProgram entrypoint program
    ctx.dispatch $ SetProgramCounter (Array.length text)
    ctx.dispatch $ SetRuntimeState Runtime.Loaded
