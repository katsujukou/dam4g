module DAM4G.Compiler.Syntax.Error
  ( ParseError
  , ParseErrorDesc(..)
  , PositionedError
  , SyntaxError(..)
  ) where

import Prelude

import DAM4G.Compiler.Global as G
import DAM4G.Compiler.Name (ConstructorName, Ident, ModuleName, OperatorName, TypeName)
import DAM4G.Compiler.Syntax.CST (SourceToken, Token)
import DAM4G.Compiler.Syntax.Source (SourceLoc, SourcePhrase, SourcePos)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data ParseErrorDesc
  = LexUnexpected String
  | UnexpectedToken Token
  | NotAConstructorName String

derive instance genericParseErrorDesc :: Generic ParseErrorDesc _
instance showParseErrorDesc :: Show ParseErrorDesc where
  show = genericShow

type PositionedError =
  { desc :: ParseErrorDesc
  , pos :: SourcePos
  }

type ParseError =
  { desc :: ParseErrorDesc
  , at :: SourceLoc
  , toks :: Array SourceToken
  }

data SyntaxError
  = UnboundOperator OperatorName
  | UnboundName Ident
  | UnboundTypeName TypeName
  | NotAConstructor (Maybe ModuleName) Ident
  | UnknownConstructor (Maybe ModuleName) ConstructorName
  | InvalidTypName SourceLoc Ident
  | InvalidConstructorName SourceLoc Ident
  | ModuleDoesNotExportName ModuleName Ident
  | NotSupportedYet String
  | OperatorsAssociativityConflicts (Array (SourcePhrase () G.OperatorInfo))
  | ConstructorNameConflicts SourceLoc ConstructorName

derive instance Generic SyntaxError _
instance Show SyntaxError where
  show = genericShow