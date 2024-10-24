module DAM4G.Simulator.Hooks.UseStore where

import Prelude

import DAM4G.Simulator.Compiler (Compiler, GdoFile, emptyGdoFile, loadCompiler, loadGdoFile)
import DAM4G.Simulator.Compiler as Compiler
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Halogen.Helix (UseHelix, UseHelixHook, makeStore')
import Halogen.Hooks (type (<>), UseEffect, UseRef, UseState, useLifecycleEffect, useRef, useState)
import Halogen.Hooks as Hooks

data AppState
  = Start
  | Loading
  | Ready

derive instance Eq AppState
derive instance Generic AppState _
instance Show AppState where
  show = genericShow

type State =
  { appstate :: AppState
  , gasm :: GdoFile
  , compileError :: Maybe String
  }

data Action
  = SetLoading
  | SetReady
  | SetGasm GdoFile
  | SetCompileError String
  | ResetCompile

useStore :: forall m a. Eq a => MonadEffect m => UseHelixHook State Action a m
useStore = makeStore' "store" reducer initialState
  where
  initialState =
    { appstate: Start
    , gasm: emptyGdoFile
    , compileError: Nothing
    }
  reducer st = case _ of
    SetLoading -> st { appstate = Loading }
    SetReady -> st { appstate = Ready }
    SetGasm gdofile -> st { gasm = gdofile }
    SetCompileError error -> st { compileError = Just error }
    ResetCompile -> st { compileError = Nothing, gasm = emptyGdoFile }

type AppInterface m =
  { gasm :: GdoFile
  , runCompile :: String -> Hooks.HookM m Unit
  , getCompileError :: Hooks.HookM m (Maybe String)
  , getGasm :: Hooks.HookM m GdoFile
  }

useApp :: forall m. MonadAff m => Hooks.Hook m (UseHelix State <> UseRef (Maybe Compiler) <> UseEffect <> Hooks.Pure) (AppInterface m)
useApp = Hooks.do
  state /\ ctx <- useStore identity
  _ /\ compilerRef <- useRef Nothing
  useLifecycleEffect do
    ctx.dispatch SetLoading
    void $ Hooks.fork do
      compiler <- liftAff loadCompiler
      liftEffect $ Ref.write (Just compiler) compilerRef
      ctx.dispatch SetReady
    pure Nothing

  Hooks.pure
    { gasm: state.gasm
    , getCompileError: ctx.getState <#> _.compileError
    , getGasm: ctx.getState <#> _.gasm
    , runCompile: \src -> do
        ctx.dispatch ResetCompile
        comp <- liftEffect $ Ref.read compilerRef
        case comp of
          Nothing -> liftEffect $ throw "Compiler is not loaded"
          Just compiler -> do
            liftEffect (Compiler.compile compiler src) >>= case _ of
              Left err -> ctx.dispatch $ SetCompileError err
              Right { emitBin } -> do
                gdofile <- liftEffect (emitBin >>= loadGdoFile)
                ctx.dispatch $ SetGasm gdofile
    }