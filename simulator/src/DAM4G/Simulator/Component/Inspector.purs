module DAM4G.Simulator.Component.Inspector where

import Prelude

import DAM4G.Simulator.Byte (print2Byte, printByte)
import DAM4G.Simulator.Component.Asset (assetUrls, toString)
import DAM4G.Simulator.Component.HeapTable as HeapTable
import DAM4G.Simulator.Hooks.UseStore (useApp)
import DAM4G.Simulator.Runtime (Value(..))
import Data.Array ((!!))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

make :: forall q i o m. MonadAff m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  { runtime, program } <- useApp
  let
    ctx =
      { runtime: runtime
      , pgc: case program.code !! runtime.cpu.pgc of
          Nothing -> (-1)
          Just (n /\ _) -> n
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div []
      [ HH.text $ show ctx.runtime.cpu
      , HH.h2 [ HP.class_ $ ClassName "flex" ]
          [ HH.img
              [ HP.src $ toString assetUrls.icons.inspector
              , HP.width 16
              , HP.class_ $ ClassName "mr-1"
              ]
          , HH.text "Inspector"
          ]
      , HH.div []
          [ HH.h3
              [ HP.class_ $ ClassName "flex" ]
              [ HH.img
                  [ HP.src $ toString assetUrls.icons.cpu
                  , HP.width 16
                  , HP.class_ $ ClassName "mr-1"
                  ]
              , HH.text "CPU"
              ]
          , HH.table [ HP.class_ $ ClassName "border font-HackGenNF" ]
              [ HH.tbody_
                  [ HH.tr [ HP.class_ $ ClassName "border border-b-gray-100" ]
                      [ HH.td
                          [ HP.class_ $ ClassName "" ]
                          [ HH.text "PGC" ]
                      , HH.td
                          [ HP.class_ $ ClassName "" ]
                          [ HH.text "=" ]
                      , HH.td
                          [ HP.class_ $ ClassName "text-blue-400" ]
                          [ HH.text $ print2Byte ctx.pgc ]
                      ]
                  , HH.tr [ HP.class_ $ ClassName "border border-b-gray-100" ]
                      [ HH.td
                          []
                          [ HH.text "ACC" ]
                      , HH.td
                          [ HP.class_ $ ClassName "" ]
                          [ HH.text "=" ]
                      , HH.td
                          [ HP.class_ $ ClassName "text-blue-400" ]
                          [ printValue ctx.runtime.cpu.acc ]
                      ]
                  , HH.tr [ HP.class_ $ ClassName "border border-b-gray-100" ]
                      [ HH.td
                          []
                          [ HH.text "ENV" ]
                      , HH.td
                          [ HP.class_ $ ClassName "" ]
                          [ HH.text "=" ]
                      , HH.td
                          [ HP.class_ $ ClassName "text-blue-400" ]
                          [ HH.text $ show ctx.runtime.cpu.env
                          ]
                      ]
                  , HH.tr [ HP.class_ $ ClassName "border border-b-gray-100" ]
                      [ HH.td
                          []
                          [ HH.text "ARG" ]
                      , HH.td
                          [ HP.class_ $ ClassName "" ]
                          [ HH.text "=" ]
                      , HH.td
                          [ HP.class_ $ ClassName "text-blue-400" ]
                          [ HH.text $ show ctx.runtime.cpu.arg ]
                      ]
                  , HH.tr []
                      [ HH.td
                          []
                          [ HH.text "RET" ]
                      , HH.td
                          [ HP.class_ $ ClassName "" ]
                          [ HH.text "=" ]
                      , HH.td
                          []
                          [ HH.text $ show ctx.runtime.cpu.ret ]
                      ]
                  ]
              ]
          ]
      , HH.div [ HP.class_ $ ClassName "" ]
          [ HH.div [ HP.class_ $ ClassName "" ]
              [ HH.h3
                  [ HP.class_ $ ClassName "flex" ]
                  [ HH.img
                      [ HP.src $ toString assetUrls.icons.memory
                      , HP.width 16
                      , HP.class_ $ ClassName "mr-1"
                      ]
                  , HH.text "Heap"
                  ]
              , HH.slot (Proxy :: _ "heap") unit HeapTable.make {} absurd
              ]
          --   , HH.div [ HP.class_ $ ClassName "col-span-1" ]
          --       [ HH.h3
          --           [ HP.class_ $ ClassName "flex" ]
          --           [ HH.img
          --               [ HP.src $ toString assetUrls.icons.table
          --               , HP.width 16
          --               , HP.class_ $ ClassName "mr-1"
          --               ]
          --           , HH.text "Globals"
          --           ]
          --       , renderGlobals ctx
          --       ]
          ]
      ]

  printValue = case _ of
    Imd n -> do
      HH.div []
        [ HH.div
            [ HP.class_ $ ClassName "text-sm text-white bg-red-700 rounded-sm p-1"
            ]
            [ HH.text "Immd" ]
        , HH.span []
            [ HH.text $ printByte n ]
        ]
    Ptr addr -> do
      HH.div []
        [ HH.div
            [ HP.class_ $ ClassName "text-sm text-white bg-blue-700 rounded-sm p-1"
            ]
            [ HH.text "Addr" ]
        , HH.span []
            [ HH.text $ print2Byte addr ]
        ]
    Epsiron -> do
      HH.div [ HP.class_ $ ClassName "text-purple-500" ]
        [ HH.text "ε" ]
    Uninitialized -> do
      HH.div [ HP.class_ $ ClassName "text-gray-500" ]
        [ HH.text "****" ]

--   renderGlobals ctx = do
--     HH.table [] $
--       ctx.runtime.globals
--         # Map.toUnfoldable
--         <#> \(sym /\ val) -> do
--           HH.tr
--             []
--             [ HH.td [] [ HH.text sym ]
--             , HH.td [] [ HH.text $ show val ]
--             ]