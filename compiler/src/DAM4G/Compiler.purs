module DAM4G.Compiler
  ( CompileInput(..)
  , CompileOutput
  , compile
  , compileJs
  ) where

import Prelude

import DAM4G.Compiler.Backend.CodeGen (emit, mkGdoFile)
import DAM4G.Compiler.Backend.Lower as Backend
import DAM4G.Compiler.Base (baseEnv)
import DAM4G.Compiler.Optimizer.Translate as TR
import DAM4G.Compiler.Syntax.CST (printToken)
import DAM4G.Compiler.Syntax.Error as SyntaxError
import DAM4G.Compiler.Syntax.Parser as Parser
import DAM4G.Syntax.WellFormedness as WF
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, null, toNullable)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console

data CompileInput
  = InpString String
  | InpFile String

type CompileOutput =
  { dasmText :: Effect String
  , emitBin :: Effect ArrayBuffer
  }

type CompileResult = Either String CompileOutput

compile :: String -> Effect CompileResult
compile src = do
  case Parser.parse "Sample" src of
    Left err -> pure $ Left $ describeParseError err
    Right cstModule -> do
      let Identity res = WF.runCheck baseEnv (WF.checkModule cstModule)
      case res of
        Left err -> pure $ Left $ describeCheckError err
        Right (astModule /\ genv) -> do

          irModule /\ _ <- TR.translate genv astModule
          pure $ Right
            { dasmText: pure "program"
            , emitBin: irModule # Backend.lower >>> mkGdoFile >>> emit
            }
  where
  describeParseError { at, desc } =
    "An error has occurred during parsing:\n  " <>
      case desc of
        SyntaxError.LexUnexpected str -> "Unexpected input: " <> str
        SyntaxError.UnexpectedToken tok -> "Unexpected token: " <> printToken tok

  describeCheckError err = "An error has occurred during well-formedness cheking:\n  " <>
    case err of
      SyntaxError.UnboundOperator op -> "Unbound operator: " <> unwrap op
      SyntaxError.UnboundName name -> "Unbound name: " <> unwrap name
      SyntaxError.UnboundTypeName typname -> "Unbound type name: " <> unwrap (unwrap typname)
      SyntaxError.NotAConstructor _ name -> "Not a valid constructor name: " <> unwrap name
      SyntaxError.UnknownConstructor _ ctorname -> "Unknown constructor name: " <> unwrap (unwrap ctorname)
      SyntaxError.InvalidTypName _ name -> "Invalid type name: " <> unwrap name
      SyntaxError.InvalidConstructorName _ ctorname -> "Invalid constructor name: " <> unwrap ctorname
      SyntaxError.ModuleDoesNotExportName modname name -> "The Module " <> unwrap modname <> " does not export " <> unwrap name
      SyntaxError.NotSupportedYet feat -> "I'm VERRRY sorry, but the feature " <> feat <> " is not supported yet."
      SyntaxError.OperatorsAssociativityConflicts _ -> "The operator associativity conflicts."
      SyntaxError.ConstructorNameConflicts _ _ -> "Constructor name conflicts"

-- For use from JavaScript.
compileJs
  :: String
  -> Effect
       { success :: Boolean
       , error :: Nullable String
       , output :: Effect ArrayBuffer
       }
compileJs = compile >=> case _ of
  Left err -> pure { success: false, error: toNullable (Just err), output: AB.empty 0 }
  Right { emitBin } -> pure { success: true, error: null, output: emitBin }
