module DAM4G.Simulator.Main where

import Prelude

import DAM4G.Simulator.App as App
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody 
  runUI App.make {} body