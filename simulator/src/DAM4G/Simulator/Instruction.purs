module DAM4G.Simulator.Instruction where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data CodeLabel = CodeLabel String Int

derive instance Eq CodeLabel
derive instance Ord CodeLabel
derive instance Generic CodeLabel _
instance Show CodeLabel where
  show = genericShow

data Constant
  = CstBool Boolean
  | CstInt Int

derive instance Eq Constant
derive instance Generic Constant _
instance Show Constant where
  show = genericShow

type ConstructorTag = Int

data Instruction
  = KStop
  | KExit
  | KNoop
  | KLabel Int Int
  -- Constant and literals 
  | KQuote Constant
  | KGetGlobal (Either Int String)
  | KSetGlobal (Either Int String)
  | KField Int
  | KMakeBlock ConstructorTag Int
  -- Function handling
  | KClosure (Either CodeLabel Int)
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
  | KBranch Int
  | KBranchIf Int
  | KBranchIfNot Int
  | KBranchIfNotImm Constant (Either CodeLabel Int)
  | KBranchIfNotTag ConstructorTag (Either CodeLabel Int)
  | KBranchIfEqTag ConstructorTag (Either CodeLabel Int)
  | KUndefined Int

derive instance Eq Instruction
derive instance Generic Instruction _
instance Show Instruction where
  show = genericShow

