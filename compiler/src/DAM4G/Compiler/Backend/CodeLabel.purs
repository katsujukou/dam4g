module DAM4G.Compiler.Backend.CodeLabel where

import Prelude

import Data.Newtype (class Newtype)
import Fmt as Fmt

newtype CodeLabel = CodeLabel Int

derive instance Eq CodeLabel
derive instance Ord CodeLabel
derive instance Newtype CodeLabel _
instance Show CodeLabel where
  show (CodeLabel lbl) = Fmt.fmt @"(CodeLabel {lbl})" { lbl }

type Labeled a =
  { lbl :: CodeLabel
  , it :: a
  }

newtype ByteOffset = ByteOffset Int

derive instance Newtype ByteOffset _

