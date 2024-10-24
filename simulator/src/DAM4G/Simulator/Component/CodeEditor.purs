module DAM4G.Simulator.Component.CodeEditor where

import Prelude

import DAM4G.Simulator.Component.Button as Button
import DAM4G.Simulator.Hooks.UseStore (useApp)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

data Output = CompileTriggered String

make :: forall q i m. MonadAff m => H.Component q i Output m
make = Hooks.component \{ outputToken } _ -> Hooks.do
  src /\ srcId <- useState ""
  let
    handleCompile = case _ of
      Button.Clicked -> do
        src' <- Hooks.get srcId
        Hooks.raise outputToken $
          CompileTriggered src'
    ctx =
      { src
      , setSrc: Hooks.put srcId
      , handleCompile
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div []
      [ HH.div [ HP.class_ $ ClassName "flex flex-row-reverse" ]
          [ HH.slot (Proxy :: _ "button") 1 Button.make
              { label: "Compile", outerClass: Nothing }
              ctx.handleCompile
          ]
      , HH.div []
          [ HH.text "Code editor "
          ]
      ]
