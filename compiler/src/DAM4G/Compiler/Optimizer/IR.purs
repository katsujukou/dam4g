module DAM4G.Compiler.Optimizer.IR where

import Prelude

import DAM4G.Compiler.Name (Ident, ModuleName)
import DAM4G.Compiler.Primitive (Primitive)
import DAM4G.Compiler.Types (AtomicConstant, ConstructorTag, StructuredConstant)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Fmt as Fmt

type Occurrence = Int

type Path = List Occurrence

newtype Var = Var Int

derive instance Newtype Var _

instance Show Var where
  show (Var var) = Fmt.fmt @"(Var {var})" { var: show var }

data ELC a
  = ELConst a StructuredConstant
  | ELVar a Var
  | ELPrim a Primitive (Array (ELC a))
  | ELAbs a Int (ELC a)
  | ELApp a (ELC a) (Array (ELC a))
  | ELLet a (Array (ELC a)) (ELC a)
  | ELLetRec a (Array (ELC a)) (ELC a)
  | ELCond a (ELC a) (Array (AtomicConstant /\ ELC a))
  | ELSwitch a (ELC a) (Array (ConstructorTag /\ ELC a))
  | ELIf a (ELC a) (ELC a) (ELC a)
  | ELRaise a
  | ELTrap a (ELC a) (ELC a)

derive instance Functor ELC
derive instance Generic (ELC a) _
instance Show a => Show (ELC a) where
  show it = genericShow it

type BlockTag = Int

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

derive instance Newtype Module _
instance Show Module where
  show (Module m) = Fmt.fmt @"(Module {m})" { m: show m }
