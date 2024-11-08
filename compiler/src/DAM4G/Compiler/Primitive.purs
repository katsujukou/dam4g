module DAM4G.Compiler.Primitive where

import Prelude

import DAM4G.Compiler.Name (Ident, ModuleName)
import DAM4G.Compiler.Types (BlockTag)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Primitive
  = PGetGlobal ModuleName Ident
  | PSetGlobal ModuleName Ident
  | PField Int
  | PMakeBlock BlockTag
  -- Integer arithmetic
  | P_i32_add
  | P_i32_sub
  | P_i32_mul
  | P_i32_div
  | P_i32_mod
  | P_i32_equ
  | P_i32_neq
  | P_i32_le -- less than or equal to
  | P_i32_lt -- less than
  -- Logical operations
  | P_log_and -- logical and 
  | P_log_or -- logical or
  | P_log_not -- logical not
  | P_log_xor -- logical exclusive or 

derive instance Eq Primitive
derive instance Generic Primitive _

instance Show Primitive where
  show = genericShow

isArithmetic :: Primitive -> Boolean
isArithmetic = case _ of
  P_i32_add -> true
  P_i32_sub -> true
  P_i32_mul -> true
  P_i32_div -> true
  P_i32_mod -> true
  P_i32_equ -> true
  P_i32_neq -> true
  P_i32_le -> true
  P_i32_lt -> true
  _ -> false

isLogical :: Primitive -> Boolean
isLogical = case _ of
  P_log_and -> true
  P_log_or -> true
  P_log_not -> true
  P_log_xor -> true
  _ -> false
