module DAM4G.Simulator.Component.StackTable where

import Prelude

import DAM4G.Simulator.Byte (print2Byte)
import DAM4G.Simulator.Component.Asset (assetUrls, toString)
import DAM4G.Simulator.Runtime as RT
import Data.Array ((..))
import Data.Array as Array
import Data.List (List)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafeCrashWith)

type Inputs = { label :: String, stack :: List RT.Value }

make :: forall q o m. H.Component q Inputs o m
make = Hooks.component \_ inputs -> Hooks.do

  let
    stack = Array.fromFoldable inputs.stack
    ctx =
      { label: inputs.label
      , stack: stack <> (const RT.Uninitialized <$> (1 .. 16))
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div [ HP.class_ $ ClassName "h-full py-2" ]
      [ HH.div [ HP.class_ $ ClassName "h-full flex flex-col" ]
          [ HH.h2 [ HP.class_ $ ClassName "flex text-orange-700" ]
              [ HH.img
                  [ HP.src $ toString assetUrls.icons.stack
                  , HP.width 16
                  , HP.class_ $ ClassName "mr-1"
                  ]
              , HH.text ctx.label
              ]
          , HH.div [ HP.class_ $ ClassName "basis-0 flex-auto flex flex-col overflow-y-auto border border-gray-300 bg-gray-100" ] $
              renderTableLines ctx.stack
          ]
      ]

renderTableLines :: forall w i. Array RT.Value -> Array (HH.HTML w i)
renderTableLines values = values <#> \val -> do
  HH.div
    [ HP.class_ $ ClassName $
        "border border-b-gray-200 " <>
          (if val == RT.Uninitialized then "bg-gray-100 " else "bg-white hover:bg-yellow-50")
    ]
    [ renderValue val ]
  where
  renderValue val = do
    let
      rowCls = "flex font-HackGenNF gap-4 "
    case val of
      RT.Imd n -> do
        HH.div [ HP.class_ $ ClassName rowCls ]
          [ renderTag val
          , HH.span [ HP.class_ $ ClassName "flex-grow text-sky-600" ] [ HH.text $ show n ]
          ]
      RT.Ptr p -> do
        HH.div [ HP.class_ $ ClassName rowCls ]
          [ renderTag val
          , HH.span [ HP.class_ $ ClassName "flex-grow text-sky-600" ] [ HH.text $ print2Byte p ]
          ]

      RT.Epsiron -> do
        HH.div [ HP.class_ $ ClassName rowCls ]
          [ renderTag val
          , HH.span [ HP.class_ $ ClassName "flex-grow text-gray-400" ] [ HH.text " -- " ]
          ]

      RT.Uninitialized -> do
        HH.div [ HP.class_ $ ClassName rowCls ]
          [ HH.span [ HP.class_ $ ClassName "flex-grow text-gray-100" ] [ HH.text "unused" ]
          ]
  renderTag v = do
    HH.div [ HP.class_ $ ClassName "w-12 flex items-center" ]
      [ let
          cls = "w-10 text-center rounded-lg text-xs text-white px-1 mx-auto "
        in
          case v of
            RT.Imd _ -> do
              HH.span [ HP.class_ $ ClassName $ cls <> "bg-red-600 " ]
                [ HH.text "Imd" ]
            RT.Ptr _ -> do
              HH.span [ HP.class_ $ ClassName $ cls <> "bg-blue-700 " ]
                [ HH.text "Ptr" ]
            RT.Epsiron -> do
              HH.span [ HP.class_ $ ClassName $ cls <> "bg-purple-600 " ]
                [ HH.text "Mark" ]
            RT.Uninitialized -> HH.text ""
      ]