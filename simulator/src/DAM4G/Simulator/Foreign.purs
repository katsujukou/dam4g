module DAM4G.Simulator.Foreign where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Web.HTML (HTMLElement)

foreign import unsafeInnerHtmlImpl :: EffectFn2 HTMLElement String Unit

unsafeInnerHtml :: HTMLElement -> String -> Effect Unit
unsafeInnerHtml = runEffectFn2 unsafeInnerHtmlImpl