module DAM4G.Compiler.Backend.Program where

import Prelude

import DAM4G.Compiler.Backend.Instruction (Instruction)
import DAM4G.Compiler.Name (Ident)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Fmt as Fmt

data CodeSection = CodeSection
  { lbl :: String
  , code :: Array Instruction
  }

codeLength :: CodeSection -> Int 
codeLength (CodeSection { code }) = Array.length code 

derive instance Generic CodeSection _ 
instance Show CodeSection where
  show = genericShow

newtype Program = Program
  { name :: String
  , text :: Array CodeSection
  , data :: Array Ident
  , init :: Array Instruction
  , refs :: Array Ident
  , syms :: Array Ident
  }

derive instance Newtype Program _
instance Show Program where
  show (Program p) = Fmt.fmt @"(Program {p})" { p: show p }

empty :: String -> Program
empty name = Program
  { name
  , text: []
  , data: []
  , init: [] -- Imidiately stops execution.
  , refs: []
  , syms: []
  }