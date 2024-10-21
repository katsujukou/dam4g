module DAM4G.Compiler.Backend.Instruction where

import Prelude

import DAM4G.Compiler.Backend.CodeLabel (CodeLabel(..))
import DAM4G.Compiler.Name (Ident)
import DAM4G.Compiler.Value (Constant)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data Instruction
  = KStop
  | KNoop
  | KLabel (Maybe String) CodeLabel
  | KStartFun
  -- Constant and literals 
  | KQuote Constant
  | KGetGlobal Ident
  | KSetGlobal Ident
  | KField Int
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
  | KEndLet
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
  | KBranchNot CodeLabel

derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

-- opcode :: Instruction -> Int 
-- opcode = case _ of 
--   KNoop -> 0xFE
--   KStop -> 0xFF
--   KLabel _ -> 0x00
--   KStartFun -> 0xF0

--   KQuote _ -> 0xC0
--   KGetGlobal _ -> 0xC1 
--   KSetGlobal _ -> 0xC2
--   KField _ -> 0xC3

--   KClosure _ -> 0x80
--   KApply -> 0x81
--   KTailApply -> 0x82
--   KPush -> 0x83
--   KPushMark -> 0x84
--   KGrab -> 0x85 
--   KReturn -> 0x60

--   KAccess _ -> 0xE1
--   KLet -> 0xE2
--   KEndLet -> 0xE3
--   KDummies _ -> 0xE4
--   KUpdate _ -> 0xE5

--   K_i32_add -> 0xA1
--   K_i32_sub -> 0xA2
--   K_i32_mul -> 0xA3
--   K_i32_div -> 0xA4
--   K_i32_mod -> 0xA5
--   K_i32_equ -> 0xA6
--   K_i32_neq -> 0xA7
--   K_i32_le -> 0xA8
--   K_i32_lt -> 0xA9

--   K_log_and -> 0xAA
--   K_log_or -> 0xAB
--   K_log_xor -> 0xAC
--   K_log_not -> 0xAD

--   KBranch _ -> 0xB0
--   KBranchIf _ -> 0xB1 
--   KBranchNot _ -> 0xB2
