module DAM4G.Compiler.Syntax.Lexer where

import Prelude

import Control.Alt (class Alt, (<|>))
import DAM4G.Compiler.Name (Ident(..))
import DAM4G.Compiler.Syntax.CST (Keyword(..), SourceToken, Token(..))
import DAM4G.Compiler.Syntax.Error (ParseErrorDesc(..), PositionedError)
import DAM4G.Compiler.Syntax.Source (SourcePos, advancePos, charDelta, from, mapPhrase, stringDelta, (..), (@@), (~))
import DAM4G.Compiler.Syntax.Source as Source
import Data.Array (length)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.String.CodeUnits as SCU
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Fmt as Fmt
import Partial.Unsafe (unsafeCrashWith)

type SourcePhrase a = Source.SourcePhrase () a

type LexerState = { src :: String, pos :: SourcePos }

data LexResult a
  = LexSuccess a String SourcePos
  | LexFail PositionedError String

instance Show a => Show (LexResult a) where
  show = case _ of
    LexSuccess a src pos -> Fmt.fmt @"(LexSuccess {a} {src} {pos})"
      { a: show a, src: show src, pos: show pos }
    LexFail err src -> Fmt.fmt @"(LexFail {err} {src})"
      { err: show err, src: show src }

newtype Lexer a = Lexer (Fn2 SourcePos String (LexResult a))

instance functorLexer :: Functor Lexer where
  map f (Lexer k) = Lexer $ mkFn2 \pos src ->
    case runFn2 k pos src of
      LexSuccess a src' pos' -> LexSuccess (f a) src' pos'
      LexFail e src' -> LexFail e src'

instance applyLexer :: Apply Lexer where
  apply (Lexer k1) (Lexer k2) = Lexer $ mkFn2 \pos src ->
    case runFn2 k1 pos src of
      LexFail e src' -> LexFail e src'
      LexSuccess f src' pos' ->
        case runFn2 k2 pos' src' of
          LexFail e src'' -> LexFail e src''
          LexSuccess a src'' pos'' -> LexSuccess (f a) src'' pos''

instance applicativeLexer :: Applicative Lexer where
  pure a = Lexer $ mkFn2 \pos src -> LexSuccess a src pos

instance bindLexer :: Bind Lexer where
  bind (Lexer k1) f = Lexer $ mkFn2 \pos src ->
    case runFn2 k1 pos src of
      LexFail e src' -> LexFail e src'
      LexSuccess a src' pos' ->
        let
          Lexer k2 = f a
        in
          runFn2 k2 pos' src'

instance monadLexer :: Monad Lexer

instance altLexer :: Alt Lexer where
  alt (Lexer k1) (Lexer k2) = Lexer $ mkFn2 \pos0 src0 ->
    case runFn2 k1 pos0 src0 of
      LexSuccess a src1 pos1 -> LexSuccess a src1 pos1
      LexFail e src1
        | src1 /= src0 -> LexFail e src1
        | otherwise -> runFn2 k2 pos0 src0

runLexer :: forall a. Lexer a -> LexerState -> LexResult a
runLexer (Lexer k) { pos, src } = runFn2 k pos src

failwith :: forall a. PositionedError -> Lexer a
failwith err = Lexer $ mkFn2 \_ src -> LexFail err src

getCurrentState :: Lexer LexerState
getCurrentState = Lexer $ mkFn2 \pos src -> LexSuccess { pos, src } src pos

setState :: LexerState -> Lexer Unit
setState s0 = Lexer $ mkFn2 \_ _ -> LexSuccess unit s0.src s0.pos

type SourceString = SourcePhrase String

tokenize :: Lexer SourceToken
tokenize = do
  _ <- whitespace
  ( punctuation
      <|> int
      <|> ident
      <|> operator
      <|> eof
  )

--   (punctuation 
--   <|> operator 
--   <|> ident 
--   <|> eof)

whitespace :: Lexer Int
whitespace = length <$>
  many
    ( char '\n'
        <|> char '\r'
        <|> char ' '
        <|> char '\t'
    )

eof :: Lexer SourceToken
eof = Lexer $ mkFn2 \pos src ->
  if src == "" then LexSuccess (TokEOF @@ (pos ~ pos)) src pos
  else LexFail (mkUnexpected "" pos) src

punctuation :: Lexer SourceToken
punctuation = do
  s0 <- getCurrentState
  ch <- anychar
  case ch.it of
    ')' -> pure $ mapPhrase (const TokRightParens) ch
    '{' -> pure $ mapPhrase (const TokLeftBrace) ch
    '}' -> pure $ mapPhrase (const TokRightBrace) ch
    '[' -> pure $ mapPhrase (const TokLeftSquare) ch
    ']' -> pure $ mapPhrase (const TokRightSquare) ch
    ',' -> pure $ mapPhrase (const TokComma) ch
    ';' -> pure $ mapPhrase (const TokSemicolon) ch
    '(' -> do
      s1 <- getCurrentState
      nextCh <- anychar
      case nextCh.it of
        ')' -> pure (TokUnit @@ (ch.at .. nextCh.at))
        _ -> setState s1 $> mapPhrase (const TokLeftParens) ch
    _ -> setState s0 *> failwith (mkUnexpected (SCU.singleton ch.it) (from ch.at))

operator :: Lexer SourceToken
operator = do
  matched <- regex operatorRegex
  case matched.it of
    "=" -> pure $ matched { it = TokEqual }
    ":" -> pure $ matched { it = TokColon }
    "->" -> pure $ matched { it = TokRightArrow }
    "|" -> pure $ matched { it = TokPipe }
    op -> pure $ matched { it = TokOperator op }

  where
  operatorRegex = """[!#$%&=\-\^~|\\@+*:?/<>]+"""

int :: Lexer SourceToken
int = do
  matched <- regex """-?[0-9]+"""
  case Int.fromString matched.it of
    Nothing -> unsafeCrashWith "Impossible"
    Just n -> pure $ matched { it = TokInt matched.it n }

ident :: Lexer SourceToken
ident = do
  matched <- regex """[a-zA-Z0-9_][a-zA-Z0-9'_]*"""
  case parseKeyword matched.it of
    Just kw -> pure $ matched { it = TokReserved kw }
    _ -> case matched.it of
      "true" -> pure $ mapPhrase (const (TokBool matched.it true)) matched
      "false" -> pure $ mapPhrase (const (TokBool matched.it false)) matched
      "_" -> pure $ mapPhrase (const TokUnderscore) matched
      _ -> pure $ mapPhrase TokIdent $ mapPhrase Ident matched

anychar :: Lexer (SourcePhrase Char)
anychar = Lexer $ mkFn2 \pos0 src0 -> case SCU.uncons src0 of
  Nothing -> LexFail (mkUnexpected src0 pos0) src0
  Just { head: ch, tail: src1 } ->
    let
      pos1 = advancePos pos0 (charDelta ch)
    in
      LexSuccess (ch @@ (pos0 ~ pos1)) src1 pos1

char :: Char -> Lexer (SourcePhrase Char)
char ch = Lexer $ mkFn2 \pos0 src0 -> case SCU.uncons src0 of
  Nothing -> LexFail (mkUnexpected "<eof>" pos0) ""
  Just { head, tail: src1 }
    | head == ch ->
        let
          pos' = advancePos pos0 (charDelta head)
        in
          LexSuccess (ch @@ (pos0 ~ pos')) src1 pos'
    | otherwise -> LexFail (mkUnexpected src0 pos0) src0

many :: forall a. Lexer a -> Lexer (Array a)
many (Lexer k) = Lexer $ mkFn2 \pos src -> go [] { pos, src }
  where
  go ls s =
    case runFn2 k s.pos s.src of
      LexSuccess a src' pos' -> go (ls <> [ a ]) { src: src', pos: pos' }
      LexFail e src'
        | s.src == src' -> LexSuccess ls src' e.pos
        | otherwise -> LexFail e src'

mkUnexpected :: String -> SourcePos -> PositionedError
mkUnexpected src pos
  | src == "" = { desc: LexUnexpected "<eof>", pos }
  | Str.length src <= 6 = { desc: LexUnexpected src, pos }
  | otherwise = { desc: LexUnexpected $ Str.take 5 src <> "...", pos }

regex :: String -> Lexer (SourcePhrase String)
regex regexStr = Lexer $ mkFn2 \pos0 src0 ->
  case Re.match matchRegex src0 of
    Just groups
      | Just match <- NonEmptyArray.head groups ->
          let
            matchLength = SCU.length match
            pos' = advancePos pos0 (stringDelta $ SCU.take matchLength src0)
          in
            LexSuccess (match @@ (pos0 ~ pos')) (SCU.drop matchLength src0) pos'
    _ ->
      LexFail (mkUnexpected src0 pos0) src0
  where
  matchRegex = unsafeRegex ("^(?:" <> regexStr <> ")") unicode

parseKeyword :: String -> Maybe Keyword
parseKeyword = case _ of
  "fn" -> Just KW_fn
  "if" -> Just KW_if
  "then" -> Just KW_then
  "else" -> Just KW_else
  "let" -> Just KW_let
  "in" -> Just KW_in
  "rec" -> Just KW_rec
  "and" -> Just KW_and
  "match" -> Just KW_match
  "matchfn" -> Just KW_matchfn
  "with" -> Just KW_with
  "as" -> Just KW_as
  "of" -> Just KW_of
  "def" -> Just KW_def
  "type" -> Just KW_type
  "alias" -> Just KW_alias
  _ -> Nothing

