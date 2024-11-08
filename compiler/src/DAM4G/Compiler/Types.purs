module DAM4G.Compiler.Types where

import Prelude

import DAM4G.Compiler.Name (Ident, NameSource, Qualified, TypeName)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable)

data Kind a = KndType a

derive instance Functor Kind
derive instance Eq a => Eq (Kind a)
derive instance Generic (Kind a) _
instance Show a => Show (Kind a) where
  show it = genericShow it

data Type_ a
  = TGlobal a (Qualified TypeName)
  | TFunc a (Type_ a) (Type_ a)
  | TTup a (Array (Type_ a))
  | TVar a Ident

derive instance Functor Type_
derive instance Foldable Type_
derive instance Traversable Type_
derive instance Eq a => Eq (Type_ a)
derive instance Generic (Type_ a) _
instance Show a => Show (Type_ a) where
  show it = genericShow it

isFunctionType :: forall a. Type_ a -> Boolean
isFunctionType = case _ of
  TFunc _ _ _ -> true
  _ -> false

type TypeAnn =
  { src :: NameSource
  }

typeAnn :: forall a. Type_ a -> a
typeAnn = case _ of
  TGlobal a _ -> a
  TFunc a _ _ -> a
  TTup a _ -> a
  TVar a _ -> a

data Associativity = RightAssoc | LeftAssoc | NonAssoc

derive instance Eq Associativity
derive instance Generic Associativity _
instance Show Associativity where
  show = genericShow

type ConstructorTag = Int

data BlockTag
  = TTuple
  | TConstr ConstructorTag

derive instance Eq BlockTag
derive instance Generic BlockTag _
instance Show BlockTag where
  show = genericShow

data AtomicConstant
  = ACUnit
  | ACBool Boolean
  | ACInt Int

derive instance Eq AtomicConstant
derive instance Ord AtomicConstant
derive instance Generic AtomicConstant _
instance Show AtomicConstant where
  show = genericShow

data StructuredConstant
  = SCAtom AtomicConstant
  | SCBlock BlockTag (Array StructuredConstant)

derive instance Generic StructuredConstant _
derive instance Eq StructuredConstant
instance Show StructuredConstant where
  show it = genericShow it