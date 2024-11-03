module DAM4G.Compiler.Primitive where

import Prelude

import DAM4G.Compiler.Name (Ident(..), ModuleName(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Primitive
  = PGetGlobal ModuleName Ident
  | PSetGlobal ModuleName Ident
  | PAccess Int
  | PMakeBlock Int
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
derive instance Ord Primitive
derive instance Generic Primitive _

instance Show Primitive where
  show = genericShow