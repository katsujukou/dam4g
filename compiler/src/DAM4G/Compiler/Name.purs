module DAM4G.Compiler.Name where

import Prelude

import Data.Newtype (class Newtype)
import Fmt as Fmt

newtype Ident = Ident String

derive instance Eq Ident 
derive instance Ord Ident
derive instance Newtype Ident _
instance Show Ident where
  show (Ident id) = Fmt.fmt @"(Ident {id})" {id}

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _ 
instance Show ModuleName where
  show (ModuleName mn) = Fmt.fmt @"(ModuleName {mn})" {mn}