module DAM4G.Compiler.Name where

import Prelude

import Data.Newtype (class Newtype)
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Fmt as Fmt

newtype Ident = Ident String

derive instance Eq Ident
derive instance Ord Ident
derive instance Newtype Ident _
instance Show Ident where
  show (Ident id) = Fmt.fmt @"(Ident {id})" { id }

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
instance Show ModuleName where
  show (ModuleName mn) = Fmt.fmt @"(ModuleName {mn})" { mn }

newtype OperatorName = OperatorName String

derive instance Newtype OperatorName _
derive instance Eq OperatorName
derive instance Ord OperatorName
instance Show OperatorName where
  show (OperatorName constr) = Fmt.fmt @"(OperatorName {constr})" { constr }

newtype ConstructorName = ConstructorName String

derive instance Newtype ConstructorName _
derive instance Eq ConstructorName
derive instance Ord ConstructorName
instance Show ConstructorName where
  show (ConstructorName constr) = Fmt.fmt @"(ConstructorName {constr})" { constr }

isConstructorName :: Ident -> Boolean
isConstructorName (Ident name) = constrRegex `Re.test` name
  where
  constrRegex = unsafeRegex """[A-Z][a-zA-Z0-9'_]*""" unicode