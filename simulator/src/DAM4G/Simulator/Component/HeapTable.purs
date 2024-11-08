module DAM4G.Simulator.Component.HeapTable where

import Prelude

import DAM4G.Simulator.Byte (print2Byte)
import DAM4G.Simulator.Component.Asset (assetUrls, toString)
import DAM4G.Simulator.Hooks.UseStore (useApp)
import DAM4G.Simulator.Runtime (ObjTag(..))
import DAM4G.Simulator.Runtime as RT
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafeCrashWith)

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
    HH.div [ HP.class_ $ ClassName "h-full py-2" ]
      [ HH.div [ HP.class_ $ ClassName "h-full flex flex-col" ]
          [ HH.h2 [ HP.class_ $ ClassName "flex text-pink-500 font-bold" ]
              [ HH.img
                  [ HP.src $ toString assetUrls.icons.memory
                  , HP.width 16
                  , HP.class_ $ ClassName "mr-1"
                  ]
              , HH.text "Heap"
              ]
          , HH.table [ HP.class_ $ ClassName "basis-0 flex-auto flex flex-col overflow-y-auto border border-gray-300 bg-gray-100 font-HackGenNF" ] $
              renderTableLines ctx.runtime.heap
          ]
      ]

renderTableLines :: forall w i. Array (Maybe RT.ObjElement) -> Array (HH.HTML w i)
renderTableLines = mapWithIndex \i elem -> do
  HH.tr
    [ HP.class_ $ ClassName $
        "border border-b-gray-200 " <>
          (if elem == Nothing then "bg-green-100 " else "bg-red-100 hover:bg-yellow-50")
    ]
    [ HH.td [ HP.class_ $ ClassName "pl-7 pr-2  bg-gray-500 text-gray-50 text-sm" ]
        [ HH.text $ print2Byte i ]
    , renderValue elem
    ]
  where
  renderValue elem = do
    HH.td [ HP.class_ $ ClassName "px-3" ]
      [ case elem of
          Nothing -> HH.text ""
          Just v -> case v of
            RT.Tag tag sz -> do
              let
                tagLabel = case tag of
                  TClos -> "CLOSURE"
                  TBlk n -> "BLOCK:" <> show n
              HH.div
                [ HP.class_ $ ClassName "flex gap-3 " ]
                [ HH.span [ HP.class_ $ ClassName " rounded-lg px-2 text-white bg-pink-500 font-bold text-xs " ]
                    [ HH.text tagLabel ]
                , HH.span [ HP.class_ $ ClassName "text-xs text-pink-500" ]
                    [ HH.text $ "size = " <> show sz ]
                ]
            RT.Component val -> case val of
              RT.Imd n -> do
                HH.div
                  [ HP.class_ $ ClassName "flex gap-3 " ]
                  [ HH.span [ HP.class_ $ ClassName " rounded-lg px-2 text-white bg-red-700 font-bold text-xs " ]
                      [ HH.text "Imd" ]
                  , HH.span [ HP.class_ $ ClassName "text-xs text-pink-500" ]
                      [ HH.text $ "size = " <> show n ]
                  ]
              RT.Ptr p -> do
                HH.div
                  [ HP.class_ $ ClassName "flex gap-3 " ]
                  [ HH.span [ HP.class_ $ ClassName " rounded-lg px-2 text-white bg-blue-700 font-bold text-xs " ]
                      [ HH.text "Ptr" ]
                  , HH.span [ HP.class_ $ ClassName "text-xs text-sky-700" ]
                      [ HH.text $ print2Byte p ]
                  ]
              _ -> unsafeCrashWith "Impossible"
      ]
--     RT.Imd n -> do
--       HH.div [ HP.class_ $ ClassName rowCls ]
--         [ renderTag val
--         , HH.span [ HP.class_ $ ClassName "flex-grow text-sky-600" ] [ HH.text $ show n ]
--         ]
--     RT.Ptr p -> do
--       HH.div [ HP.class_ $ ClassName rowCls ]
--         [ renderTag val
--         , HH.span [ HP.class_ $ ClassName "flex-grow text-sky-600" ] [ HH.text $ print2Byte p ]
--         ]

--     RT.Epsiron -> do
--       HH.div [ HP.class_ $ ClassName rowCls ]
--         [ renderTag val
--         , HH.span [ HP.class_ $ ClassName "flex-grow text-gray-400" ] [ HH.text " -- " ]
--         ]

--     RT.Uninitialized -> do
--       HH.div [ HP.class_ $ ClassName rowCls ]
--         [ HH.span [ HP.class_ $ ClassName "flex-grow text-gray-100" ] [ HH.text "unused" ]
--         ]
-- renderTag v = do
--   HH.div [ HP.class_ $ ClassName "w-12 flex items-center" ]
--     [ let
--         cls = "w-10 text-center rounded-lg text-xs text-white px-1 mx-auto "
--       in
--         case v of
--           RT.Imd _ -> do
--             HH.span [ HP.class_ $ ClassName $ cls <> "bg-red-600 " ]
--               [ HH.text "Imd" ]
--           RT.Ptr _ -> do
--             HH.span [ HP.class_ $ ClassName $ cls <> "bg-blue-700 " ]
--               [ HH.text "Ptr" ]
--           RT.Epsiron -> do
--             HH.span [ HP.class_ $ ClassName $ cls <> "bg-purple-600 " ]
--               [ HH.text "Mark" ]
--           RT.Uninitialized -> HH.text ""
--     ]