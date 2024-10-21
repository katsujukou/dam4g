module DAM4G.Simulator.Hooks.UseStore where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen.Helix (UseHelixHook, makeStore')

type State = { value :: Maybe String }

data Action = SetValue String 

useStore :: forall m a. Eq a => MonadEffect m => UseHelixHook State Action a m
useStore = makeStore' "store" reducer initialState 
  where
    initialState = 
      { value: Nothing
      }
    reducer st = case _ of 
      SetValue v -> st { value = Just v }
    
