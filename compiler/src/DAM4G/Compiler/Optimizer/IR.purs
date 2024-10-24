module DAM4G.Compiler.Optimizer.IR where

import Prelude

import DAM4G.Compiler.Name (Ident, ModuleName)
import DAM4G.Compiler.Value (Constant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Fmt as Fmt

newtype Var = Var Int

derive instance Newtype Var _

instance Show Var where
  show (Var var) = Fmt.fmt @"(Var {var})" { var }

data ELC a
  = ELConst a Constant
  | ELVar a Var
  | ELPrim a Primitive (Array (ELC a))
  | ELAbs a Int (ELC a)
  | ELApp a (ELC a) (Array (ELC a))
  | ELLet a (Array (ELC a)) (ELC a)
  | ELLetRec a (Array (ELC a)) (ELC a)
  | ELIf a (ELC a) (ELC a) (ELC a)

derive instance Functor ELC
derive instance Generic (ELC a) _
instance Show a => Show (ELC a) where
  show it = genericShow it

type BlockTag = Int

data Primitive
  = PGetGlobal Ident
  | PSetGlobal Ident
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

derive instance Generic Primitive _

instance Show Primitive where
  show = genericShow

type Ann = Unit

newtype Decl = Decl
  { ident :: Ident
  , term :: ELC Ann
  }

derive instance Newtype Decl _ 
instance Show Decl where
  show (Decl decl) = Fmt.fmt @"{decl}" { decl: show decl }

data Declaration
  = Rec (Array Decl)
  | NonRec Decl

derive instance Generic Declaration _
instance Show Declaration where
  show it = genericShow it

declIdent :: Decl -> Ident
declIdent (Decl { ident }) = ident

newtype Module = Module
  { name :: ModuleName
  , decls :: Array Declaration
  }