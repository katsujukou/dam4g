module DAM4G.Simulator.Component.HeapTable where

import Prelude

import DAM4G.Simulator.Byte (print2Byte, printWord)
import DAM4G.Simulator.Hooks.UseStore (useApp)
import DAM4G.Simulator.Runtime as Runtime
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

make :: forall q i o m. MonadAff m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  appApi <- useApp
  let
    ctx =
      { runtime: appApi.runtime
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    let
      trClass1 = "border border-gray-300 "
      trClass2 h = case h of
        Nothing -> " bg-green-100"
        Just _ -> " bg-red-100"

    HH.table [ HP.class_ $ ClassName "w-[100%] mr-3 table table-auto border border-gray-700" ] $
      ctx.runtime.heap # mapWithIndex \addr h ->
        HH.tr
          [ HP.class_ $ ClassName $ trClass1 <> trClass2 h ]
          [ HH.td [ HP.class_ $ ClassName "flex flex-row-reverse pr-1 border-r border-r-gray-300" ]
              [ HH.span [ HP.class_ $ ClassName "text-sm text-gray-500 " ]
                  [ HH.text $ print2Byte addr ]
              ]

          , HH.td []
              []
          ]
  renderValue Nothing = do
    HH.div []
      [ HH.span [] [] ]
  renderValue (Just obj) =
    HH.div []
      [ case obj of
          Runtime.Closure fptr env -> do
            HH.div []
              [ renderTag "bg-purple-500" "Clos"
              ]
          Runtime.Cons hd tl -> do
            HH.div []
              [ renderTag "bg-orange-700" "Cons"
              ]
      ]

  renderTag classString lbl = do
    HH.span
      [ HP.class_ $ ClassName $ "m-2 text-white text-xs p-1 rounded-lg " <> classString ]
      [ HH.text lbl ]