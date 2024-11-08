module DAM4G.Simulator.Component.CodeEditor where

import Prelude

import DAM4G.Simulator.Component.Asset (assetUrls, toString)
import DAM4G.Simulator.Foreign (unsafeInnerHtml)
import DAM4G.Simulator.Hooks.UseStore (useApp)
import Data.Maybe (Maybe(..))
import Data.String.Regex as Re
import Data.String.Regex.Flags (global, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Web.DOM.Element (scrollLeft, scrollTop, setScrollLeft, setScrollTop)
import Web.HTML.HTMLElement as HTMLElement

data Output = CompileTriggered String

_codeEditorView :: H.RefLabel
_codeEditorView = H.RefLabel "code-editor-view"

_codeEditorTextarea :: H.RefLabel
_codeEditorTextarea = H.RefLabel "code-edior-textarea"

applySyntaxHighlighting :: String -> String
applySyntaxHighlighting =
  Re.replace keywordsRegex """<span class="keyword">$1</span>"""
    >>> Re.replace typenameRegex """<span class="typename">$1</span>"""
    >>> Re.replace numericRegex """<span class="numeric">$1</span>"""
    >>> Re.replace punctuationRegex """<span class="punctuation">$1</span>"""
    >>> Re.replace operatorRegex """<span class="operator">$1</span>"""
  where
  keywordsRegex = unsafeRegex
    """\b(def\s+set|def\s+alias|let|rec|in|of|assoc|right|left|prec|and|fn|match|with|matchfn|as|if|then|else)\b"""
    (unicode <> global)

  typenameRegex = unsafeRegex
    """\b([A-Z][a-z0-9A-Z\\'_]*)\b"""
    (unicode <> global)

  operatorRegex = unsafeRegex
    """([$+*#@&%!?]+)"""
    (unicode <> global)

  numericRegex = unsafeRegex
    """\b(-?\d+)\b"""
    (unicode <> global)

  punctuationRegex = unsafeRegex
    """([(|)])"""
    (unicode <> global)

srctext :: String
srctext =
  """let add3 (x: Int) (y: Int) (z: Int) = x + y + z;

let it = add3 1 2 3;
"""

make :: forall q i m. MonadAff m => H.Component q i Output m
make = Hooks.component \{ outputToken } _ -> Hooks.do
  src /\ srcId <- useState ""
  appApi <- useApp

  let
    handleCompile = do
      src' <- Hooks.get srcId
      Hooks.raise outputToken $
        CompileTriggered src'

    handleTextInput str = do
      mbCodeViewEl <- Hooks.getRef _codeEditorView
      case mbCodeViewEl >>= HTMLElement.fromElement of
        Just codeviewEl -> do
          liftEffect do
            unsafeInnerHtml codeviewEl ""
            unsafeInnerHtml codeviewEl (applySyntaxHighlighting str)
        _ -> pure unit
      Hooks.put srcId str

    handleTextareaScroll _ = do
      textareaEl <- Hooks.getRef _codeEditorTextarea
      codeviewEl <- Hooks.getRef _codeEditorView
      case textareaEl, codeviewEl of
        Just el1, Just el2 -> liftEffect do
          scrlY <- scrollTop el1
          el2 # setScrollTop scrlY
          scrlX <- scrollLeft el1
          el2 # setScrollLeft scrlX
        _, _ -> pure unit

  useLifecycleEffect do
    handleTextInput srctext

    pure Nothing

  let
    ctx =
      { src
      , setSrc: Hooks.put srcId
      , handleCompile
      , handleTextInput
      , handleTextareaScroll
      , console: appApi.console
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div
      [ HP.class_ $ ClassName "flex flex-col h-full ml-3" ]
      [ HH.div [ HP.class_ $ ClassName "h-8 my-2 flex flex-row-reverse " ]
          [ HH.button
              [ HP.class_ $ ClassName $
                  "p-1 flex items-center  rounded \
                  \bg-sky-700 gap-1 border border-sky-900 "
              , HE.onClick \_ -> ctx.handleCompile
              ]
              [ HH.img
                  [ HP.class_ $ ClassName ""
                  , HP.width 16
                  , HP.src $ toString assetUrls.icons.compile
                  ]
              , HH.span [ HP.class_ $ ClassName "text-white text-sm " ] [ HH.text "Compile" ]
              ]
          ]
      , HH.div [ HP.class_ $ ClassName "flex relative" ]
          [ HH.div [ HP.class_ $ ClassName "bg-gray-300 w-[48px]" ]
              []
          , HH.pre
              [ HP.class_ $ ClassName "w-[calc(100%-48px)] h-[480px] overflow-auto font-HackGenNF code-editor-view pl-2 overflow-auto border border-gray-100"
              , HP.ref _codeEditorView
              ]
              []
          , HH.textarea
              [ HP.class_ $ ClassName "w-[calc(100%-48px)] resize-none whitespace-pre h-[480px] absolute top-0 left-[48px] border-0 outline-none font-HackGenNF m-0 p-0 pl-2"
              , HP.style "color: transparent; background-color: transparent; caret-color: black;"
              , HE.onValueInput ctx.handleTextInput
              , HE.onScroll ctx.handleTextareaScroll
              , HP.value ctx.src
              , HP.ref _codeEditorTextarea
              ]
          ]
      , HH.div [ HP.class_ $ ClassName "my-3 flex-auto basis-0 overflow-scroll bg-gray-700 text-gray-50  font-HackGenNF" ]
          $ ctx.console <#> \msg -> do
              HH.pre_
                [ HH.text $ "DAM4G> " <> msg <> "\n" ]
      ]
