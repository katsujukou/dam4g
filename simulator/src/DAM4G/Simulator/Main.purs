module DAM4G.Simulator.Main where

import Prelude

import DAM4G.Simulator.App as App
import DAM4G.Simulator.Compiler (compile, loadCompiler, loadGdoFile)
import Data.ArrayBuffer.Typed (whole)
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (Int8Array)
import Data.Either (Either(..))
import Data.Int (hexadecimal, toStringAs)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI App.make {} body