module DAM4G.Compiler.Backend.CodeLabel where

import Prelude

import Data.Newtype (class Newtype)
import Fmt as Fmt

newtype CodeLabel = CodeLabel Int 

derive instance Eq CodeLabel 
derive instance Newtype CodeLabel _
instance Show CodeLabel where
  show (CodeLabel lbl) = Fmt.fmt @"(CodeLabel {lbl})" { lbl }

newtype ByteOffset = ByteOffset Int 

type Labeled a =
  { lbl :: CodeLabel
  , it :: a 
  }

derive instance Newtype ByteOffset _ 
-- instance Show ByteOffset where
--   show (ByteOffset ofs) = Fmt.fmt @"(ByteOffset {ofs})" { ofs: toHex ofs }
--     where
--       toHex = toStringAs hexadecimal >>> padStart 0 "0000" >>> 

