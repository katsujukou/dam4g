module DAM4G.Compiler.Backend.CodeLabel where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Fmt as Fmt

data CodeLabel = CodeLabel (Maybe String) Int

derive instance Eq CodeLabel
derive instance Ord CodeLabel
derive instance Generic CodeLabel _
instance Show CodeLabel where
  show (CodeLabel ns lbl) = Fmt.fmt @"(CodeLabel {ns} {lbl})" { ns: show ns, lbl }

type Labeled a =
  { lbl :: CodeLabel
  , it :: a
  }

newtype ByteOffset = ByteOffset Int

derive instance Newtype ByteOffset _

