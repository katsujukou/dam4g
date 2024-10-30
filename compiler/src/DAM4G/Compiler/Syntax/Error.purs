module DAM4G.Compiler.Syntax.Error
  ( ParseError
  , ParseErrorDesc(..)
  , PositionedError
  ) where

import Prelude

import DAM4G.Compiler.Syntax.CST (SourceToken, Token)
import DAM4G.Compiler.Syntax.Source (SourceLoc, SourcePos)
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