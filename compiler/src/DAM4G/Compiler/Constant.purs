module DAM4G.Compiler.Constant where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data AtomicConstant
  = ACUnit
  | ACBool Boolean
  | ACInt Int

derive instance Eq AtomicConstant
derive instance Generic AtomicConstant _
instance Show AtomicConstant where
  show = genericShow

data StructuredConstant
  = SCAtom AtomicConstant
  | SCList (Array StructuredConstant)

derive instance Generic StructuredConstant _
derive instance Eq StructuredConstant
instance Show StructuredConstant where
  show it = genericShow it
