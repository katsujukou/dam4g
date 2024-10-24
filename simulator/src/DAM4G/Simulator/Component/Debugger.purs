module DAM4G.Simulator.Component.Debugger where

import Prelude

import DAM4G.Simulator.Compiler (GdoFile)
import DAM4G.Simulator.Hooks.UseStore (useApp)
import DAM4G.Simulator.Instruction (Instruction(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (useState, useTickEffect)
import Halogen.Hooks as Hooks

make :: forall q i o m. MonadAff m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  appApi <- useApp

  let
    ctx =
      { gasm: appApi.gasm }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div []
      [ HH.h1 []
          [ HH.text "Object File Inspector & Debugger"
          ]
      , HH.div []
          [ HH.text $ show ctx.gasm
          ]
      ]