module DAM4G.Compiler.Syntax.AST where

import Prelude

import DAM4G.Compiler.Name (Ident)
import DAM4G.Compiler.Value (Constant)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Term a 
  = TmConst a Constant
  | TmIdent a Ident
  | TmVar a Int
  | TmAbs a (Term a)
  | TmApp a (Term a) (Term a)
  | TmIf a (Term a) (Term a) (Term a)
  | TmLet a (Term a)

derive instance Generic (Term a) _ 
instance Show a => Show (Term a) where
  show it = genericShow it 
