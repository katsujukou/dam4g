module DAM4G.Compiler.Backend.Instruction where

import Prelude

import DAM4G.Compiler.Backend.CodeLabel (CodeLabel(..))
import DAM4G.Compiler.Name (Ident)
import DAM4G.Compiler.Types (AtomicConstant, BlockTag, ConstructorTag)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Instruction
  = KStop
  | KExit
  | KNoop
  | KLabel CodeLabel
  -- Constant and literals 
  | KQuote AtomicConstant
  | KGetGlobal Ident
  | KSetGlobal Ident
  | KField Int
  | KMakeBlock BlockTag Int
  -- Function handling
  | KClosure CodeLabel
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
  | KBranchIfNotImm AtomicConstant CodeLabel
  | KBranchIfNotTag ConstructorTag CodeLabel
  | KBranchIfEqTag ConstructorTag CodeLabel

derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

