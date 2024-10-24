module DAM4G.Simulator.Component.Button where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks

type Input =
  { label :: String
  , outerClass :: Maybe String
  }

data Output = Clicked

make :: forall q m. MonadAff m => H.Component q Input Output m
make = Hooks.component \{ outputToken } inputs -> Hooks.do
  ripple /\ rippleId <- useState false
  let
    handleClick = do
      Hooks.put rippleId true
      void $ Hooks.fork do
        liftAff $ Aff.delay $ Milliseconds 500.0
        Hooks.put rippleId false

      Hooks.raise outputToken Clicked

    ctx =
      { ripple
      , handleClick
      , inputs
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    let
      outerClass =
        "relative overflow-hidden cursor-pointer bg-pink-500 text-white \
        \p-2 text-sm rounded focus:outline-none select-none "
          <> fromMaybe "" ctx.inputs.outerClass

      rippleClass =
        "absolute inset-0 rounded-full bg-white opacity-30 \
        \ animate-ping"
          <>
            if ctx.ripple then ""
            else " hidden"
    HH.div
      [ HP.class_ $ ClassName outerClass
      , HE.onClick \_ -> ctx.handleClick
      ]
      [ HH.span
          [ HP.class_ $ ClassName $ rippleClass ]
          []
      , HH.text "Compile"
      ]
