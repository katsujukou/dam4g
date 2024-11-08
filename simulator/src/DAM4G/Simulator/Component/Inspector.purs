module DAM4G.Simulator.Component.Inspector where

import Prelude

import DAM4G.Simulator.Byte (print2Byte)
import DAM4G.Simulator.Component.Asset (assetUrls, toString)
import DAM4G.Simulator.Component.HeapTable as HeapTable
import DAM4G.Simulator.Hooks.UseStore (useApp)
import DAM4G.Simulator.Runtime as RT
import Data.Array ((!!), (..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafeCrashWith)
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
    HH.div [ HP.class_ $ ClassName "flex flex-col h-full mr-4" ]
      [ HH.div [ HP.class_ $ ClassName "h-[30vh]" ]
          [ renderGlobalSymbolTable ctx
          ]
      , HH.div [ HP.class_ $ ClassName "flex-grow" ]
          [ HH.slot_ (Proxy :: _ "heap") "heap" HeapTable.make {}
          ]
      ]

  renderGlobalSymbolTable ctx = do
    HH.div [ HP.class_ $ ClassName "h-full py-2" ]
      [ HH.div [ HP.class_ $ ClassName "h-full flex flex-col" ]
          [ HH.h2 [ HP.class_ $ ClassName "flex text-pink-500 font-bold" ]
              [ HH.img
                  [ HP.src $ toString assetUrls.icons.table
                  , HP.width 16
                  , HP.class_ $ ClassName "mr-1"
                  ]
              , HH.text "Globals"
              ]
          , HH.div [ HP.class_ $ ClassName "basis-0 flex-auto flex flex-col overflow-y-auto border border-gray-300 font-HackGenNF" ]
              [ renderTableLines ((Just <$> Map.toUnfoldable ctx.runtime.globals) <> map (const Nothing) (1 .. 16))
              ]
          ]
      ]
    where
    renderTableLines entries = do
      HH.table [ HP.class_ $ ClassName " table-auto " ] $
        entries <#> case _ of
          Nothing -> do
            HH.tr [ HP.class_ $ ClassName "border border-t-0 border-b-gray-300 bg-gray-100 hover:bg-gray-200" ]
              [ HH.td [ HP.class_ $ ClassName "w-24 overflow-x-auto text-sm text-gray-400 " ]
                  [ HH.text "(empty)" ]
              , HH.td []
                  [ HH.text $ ""
                  ]
              ]
          Just (sym /\ val) -> do
            HH.tr [ HP.class_ $ ClassName "border border-t-0 border-b-gray-300 bg-white hover:bg-blue-100" ]
              [ HH.td [ HP.class_ $ ClassName "text-sm text-green-700 border border-r-gray-300" ]
                  [ HH.text $ show sym ]
              , HH.td []
                  [ let
                      cls = "w-10 text-center rounded-lg text-xs text-white px-1 mr-3 "
                    in
                      case val of
                        RT.Imd n -> do
                          HH.div [ HP.class_ $ ClassName "flex items-center" ]
                            [ HH.span [ HP.class_ $ ClassName $ cls <> "bg-red-600 " ]
                                [ HH.text "Imd" ]
                            , HH.span [ HP.class_ $ ClassName $ "text-sky-700" ]
                                [ HH.text $ show n ]
                            ]
                        RT.Ptr p -> do
                          HH.div [ HP.class_ $ ClassName " flex items-center " ]
                            [ HH.span [ HP.class_ $ ClassName $ cls <> "bg-blue-700 " ]
                                [ HH.text "Ptr" ]
                            , HH.span [ HP.class_ $ ClassName "text-sky-700" ]
                                [ HH.text $ print2Byte p ]
                            ]
                        RT.Epsiron -> unsafeCrashWith "Impossible"
                        RT.Uninitialized -> HH.text ""
                  ]
              ]