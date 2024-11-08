module DAM4G.Compiler.Name where

import Prelude

import DAM4G.Compiler.Syntax.Source (SourceLoc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Fmt as Fmt

class IsIdent a where
  toIdent :: a -> Ident

newtype Ident = Ident String

derive instance Eq Ident
derive instance Ord Ident
derive instance Newtype Ident _
instance Show Ident where
  show (Ident id) = Fmt.fmt @"(Ident {id})" { id }

instance IsIdent Ident where
  toIdent = identity

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
derive instance Eq ModuleName
derive instance Ord ModuleName
instance Show ModuleName where
  show (ModuleName mn) = Fmt.fmt @"(ModuleName {mn})" { mn }

newtype OperatorName = OperatorName String

derive instance Newtype OperatorName _
derive instance Eq OperatorName
derive instance Ord OperatorName
instance Show OperatorName where
  show (OperatorName constr) = Fmt.fmt @"(OperatorName {constr})" { constr }

newtype TypeName = TypeName Ident

derive instance Newtype TypeName _
derive instance Eq TypeName
derive instance Ord TypeName
instance Show TypeName where
  show (TypeName tn) = Fmt.fmt @"(TypeName {tn})" { tn: show tn }

instance IsIdent TypeName where
  toIdent = unwrap

identToConstructorName :: Ident -> Maybe ConstructorName
identToConstructorName id@(Ident ident)
  | upperIdentRegex `Re.test` ident = Just (ConstructorName id)
  | otherwise = Nothing

identToTypeName :: Ident -> Maybe TypeName
identToTypeName id@(Ident ident)
  | upperIdentRegex `Re.test` ident = Just (TypeName id)
  | otherwise = Nothing

upperIdentRegex :: Regex
upperIdentRegex = unsafeRegex """^[A-Z][a-zA-Z0-9'_]*$""" unicode

newtype ConstructorName = ConstructorName Ident

derive instance Newtype ConstructorName _
derive instance Eq ConstructorName
derive instance Ord ConstructorName
instance Show ConstructorName where
  show (ConstructorName constr) = Fmt.fmt @"(ConstructorName {constr})" { constr: show constr }

instance IsIdent ConstructorName where
  toIdent = unwrap

isConstructorName :: Ident -> Boolean
isConstructorName (Ident name) = upperIdentRegex `Re.test` name

data Qualified a = Qualified ModuleName a

derive instance Functor Qualified
derive instance Eq a => Eq (Qualified a)
derive instance Ord a => Ord (Qualified a)
derive instance Generic (Qualified a) _
instance Show a => Show (Qualified a) where
  show = genericShow

unqualify :: forall a. Qualified a -> a
unqualify (Qualified _ a) = a

moduleNameOfQualified :: forall a. Qualified a -> ModuleName
moduleNameOfQualified (Qualified modname _) = modname

type GlobalName = Qualified Ident

data NameSource
  = User SourceLoc
  | Compiler

derive instance Eq NameSource
derive instance Generic NameSource _
instance Show NameSource where
  show = genericShow