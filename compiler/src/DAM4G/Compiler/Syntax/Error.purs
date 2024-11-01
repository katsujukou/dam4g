module DAM4G.Compiler.Syntax.Error
  ( ParseError
  , ParseErrorDesc(..)
  , PositionedError
  , SyntaxError(..)
  ) where

import Prelude

import DAM4G.Compiler.Name (Ident(..), OperatorName(..), GlobalName)
import DAM4G.Compiler.Syntax.CST (SourceToken, Token)
import DAM4G.Compiler.Syntax.Source (SourceLoc(..), SourcePos)
import Data.Generic.Rep (class Generic)
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
  | NotAConstructor GlobalName
  | NotSupportedYet String
  | FunctionArgumentNotAnnotated SourceLoc

derive instance Generic SyntaxError _
instance Show SyntaxError where
  show = genericShow