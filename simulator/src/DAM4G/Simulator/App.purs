module DAM4G.Simulator.App where

import Prelude

import DAM4G.Simulator.Component.CodeEditor as CodeEditor
import DAM4G.Simulator.Component.Debugger as Debugger
import DAM4G.Simulator.Hooks.UseStore (Action(..), useApp, useStore)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

make :: forall q i o m. MonadAff m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  appApi <- useApp

  let
    handleCodeEditor = case _ of
      CodeEditor.CompileTriggered src -> do
        appApi.runCompile src

    ctx =
      { handleCodeEditor
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div [ HP.class_ $ ClassName "flex flex-col h-screen" ]
      [ HH.div [ HP.class_ $ ClassName "bg-pink-300 p-3 " ]
          [ HH.h1 [ HP.class_ $ ClassName "text-lg text-white" ]
              [ HH.text "DAM4G Simulator" ]
          ]
      , HH.div [ HP.class_ $ ClassName "grid grid-cols-3 flex-grow" ]
          [ HH.div [ HP.class_ $ ClassName "col-span-1" ]
              [ HH.div [ HP.class_ $ ClassName "flex flex-col-reverse h-[100%]" ]
                  [ HH.div [ HP.class_ $ ClassName "h-[160px]" ]
                      [ HH.text "console" ]
                  , HH.div [ HP.class_ $ ClassName "flex-grow" ]
                      [ HH.slot (Proxy :: _ "code-editor") unit CodeEditor.make {} ctx.handleCodeEditor ]
                  ]
              ]
          , HH.div [ HP.class_ $ ClassName "col-span-1" ]
              [ HH.slot_ (Proxy :: _ "debugger") unit Debugger.make {} ]
          , HH.div [ HP.class_ $ ClassName "col-span-1" ]
              [ HH.text "inspector" ]
          ]
      ]