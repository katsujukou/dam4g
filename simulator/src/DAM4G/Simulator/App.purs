module DAM4G.Simulator.App where

import Prelude

import DAM4G.Simulator.Component.Asset (assetUrls, toString)
import DAM4G.Simulator.Component.CodeEditor as CodeEditor
import DAM4G.Simulator.Component.Debugger as Debugger
import DAM4G.Simulator.Component.Inspector as Inspector
import DAM4G.Simulator.Hooks.UseStore (useApp)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
      [ HH.div [ HP.class_ $ ClassName "bg-pink-300 flex justify-between items-center px-3" ]
          [ HH.h1 [ HP.class_ $ ClassName "text-lg text-white" ]
              [ HH.text "DAM4G Simulator" ]
          , HH.a
              [ HP.class_ $ ClassName " cursor-pointer "
              , HP.href "https://github.com/katsujukou/dam4g"
              , HP.target "_blank"
              , HP.rel "noopener noreferrer"
              ]
              [ HH.img
                  [ HP.src $ toString assetUrls.icons.github
                  , HP.width 18
                  ]
              ]
          ]
      , HH.div [ HP.class_ $ ClassName "grid grid-cols-5 gap-5 flex-grow bg-pink-50 " ]
          [ HH.div [ HP.class_ $ ClassName "col-span-2" ]
              [ HH.slot (Proxy :: _ "code-editor") unit CodeEditor.make {} ctx.handleCodeEditor
              ]
          , HH.div [ HP.class_ $ ClassName "col-span-2" ]
              [ HH.slot_ (Proxy :: _ "debugger") unit Debugger.make {} ]
          , HH.div [ HP.class_ $ ClassName "col-span-1" ]
              [ HH.slot_ (Proxy :: _ "inspector") unit Inspector.make {}
              ]
          ]
      ]