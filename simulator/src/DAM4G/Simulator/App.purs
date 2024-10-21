module DAM4G.Simulator.App where

import Prelude

import DAM4G.Simulator.Hooks.UseStore (Action(..), useStore)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks

make :: forall q i o m. MonadEffect m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  value /\ valueId <- useState Nothing
  store /\ storeCtx <- useStore identity
  let
    ctx =
      { value
      , setValue: Hooks.put valueId
      , value2: store.value
      , setValue2: storeCtx.dispatch <<< SetValue
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div []
      [ HH.h1 []
          [ HH.text "DAM4G Simulator" ]
      , HH.div []
          [ HH.input
              [ HP.type_ InputText
              , HP.value $ fromMaybe "" ctx.value
              , HE.onValueInput (Just >>> ctx.setValue)
              ]
          , HH.div []
              [ HH.text $ show ctx.value ]
          ]
      , HH.div []
          [ HH.input
              [ HP.type_ InputText
              , HP.value $ fromMaybe "" ctx.value2
              , HE.onValueInput ctx.setValue2
              ]
          ]
      , HH.div []
          [ HH.text $ show ctx.value2 ]
      ]