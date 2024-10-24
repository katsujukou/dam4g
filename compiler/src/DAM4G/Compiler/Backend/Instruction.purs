module DAM4G.Compiler.Backend.Instruction where

import Prelude

import DAM4G.Compiler.Backend.CodeLabel (CodeLabel)
import DAM4G.Compiler.Name (Ident)
import DAM4G.Compiler.Value (Constant)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Instruction
  = KStop
  | KNoop
  | KLabel CodeLabel
  -- Constant and literals 
  | KQuote Constant
  | KGetGlobal Ident
  | KSetGlobal Ident
  | KField Int
  -- Function handling
  | KClosure String CodeLabel
  | KApply
  | KTailApply
  | KGrab
  | KPush
  | KPushMark
  | KReturn
  -- Environment handling
  | KAccess Int
  | KLet
  | KEndLet Int
  | KDummies Int
  | KUpdate Int
  -- Integer arithmetics 
  | K_i32_add
  | K_i32_sub
  | K_i32_mul
  | K_i32_div
  | K_i32_mod
  | K_i32_equ
  | K_i32_neq
  | K_i32_le
  | K_i32_lt
  -- Logical operation
  | K_log_and
  | K_log_or
  | K_log_xor
  | K_log_not
  -- Branching and conditional
  | KBranch CodeLabel
  | KBranchIf CodeLabel
  | KBranchIfNot CodeLabel

derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

