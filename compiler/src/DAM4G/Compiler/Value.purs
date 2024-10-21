module DAM4G.Compiler.Value where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Constant 
  = CstInt Int 
  | CstBool Boolean

derive instance Eq Constant 
derive instance Ord Constant 
derive instance Generic Constant _ 
instance Show Constant where
  show = genericShow
