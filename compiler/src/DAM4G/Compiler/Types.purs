module DAM4G.Compiler.Types where

import Prelude

import DAM4G.Compiler.Name (GlobalName, Ident)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable)

data Type_ a
  = TGlobal a (GlobalName)
  | TFunc a (Type_ a) (Type_ a)
  | TVar a Ident

derive instance Functor Type_
derive instance Foldable Type_
derive instance Traversable Type_
derive instance Eq a => Eq (Type_ a)
derive instance Generic (Type_ a) _
instance Show a => Show (Type_ a) where
  show it = genericShow it

data Associativity = RightAssoc | LeftAssoc | NonAssoc

derive instance Eq Associativity
derive instance Generic Associativity _
instance Show Associativity where
  show = genericShow

