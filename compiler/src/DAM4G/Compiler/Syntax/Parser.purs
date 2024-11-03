module DAM4G.Compiler.Syntax.Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Control.Monad.Rec.Class (Step(..), tailRec)
import DAM4G.Compiler.Constant (AtomicConstant(..))
import DAM4G.Compiler.Name (ConstructorName(..), Ident(..), OperatorName(..), isConstructorName)
import DAM4G.Compiler.Syntax.CST (Ann, Binder(..), Declaration(..), Expr(..), Keyword(..), Module(..), Pattern(..), PatternMatrix(..), Recursivity(..), SourceToken, Token(..), Type_(..), binderAnn, exprAnn, patternAnn, typeAnn)
import DAM4G.Compiler.Syntax.Error (ParseError, ParseErrorDesc(..))
import DAM4G.Compiler.Syntax.Lexer (LexResult(..), runLexer, tokenize)
import DAM4G.Compiler.Syntax.Source (SourcePos(..), mapPhrase, (..), (@@), (~))
import DAM4G.Compiler.Syntax.Source as Source
import DAM4G.Compiler.Types (Associativity(..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.Partial as ArrayP
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

type SourcePhrase a = Source.SourcePhrase () a

type ParserState =
  { src :: String
  , pos :: SourcePos
  }

type ParseResult a = Either ParseError a

recoverable :: ParseError -> Boolean
recoverable = _.desc >>> case _ of
  UnexpectedToken _ -> true
  NotAConstructorName _ -> true
  _ -> false

recover :: ParserState -> ParseError -> Either ParseError ParserState
recover s0 err
  | recoverable err = Right s0
  | otherwise = Left err

newtype Parser a = Parser (ParserState -> ParseResult a /\ ParserState)

instance Functor Parser where
  map f (Parser k) = Parser \s0 -> lmap (map f) (k s0)

instance Apply Parser where
  apply (Parser k1) (Parser k2) = Parser \s0 ->
    case k1 s0 of
      Left e /\ s1 -> Left e /\ s1
      Right f /\ s1 -> lmap (map f) (k2 s1)

instance Applicative Parser where
  pure a = Parser \s0 -> Right a /\ s0

instance Bind Parser where
  bind (Parser k1) f = Parser \s0 ->
    case k1 s0 of
      Left e /\ s1 -> Left e /\ s1
      Right a /\ s1 -> let Parser k2 = f a in k2 s1

instance Monad Parser

instance Alt Parser where
  alt (Parser k1) (Parser k2) = Parser \s0 ->
    case k1 s0 of
      Right a /\ s1 -> Right a /\ s1
      Left err /\ s1
        | s1 == s0 -> k2 s1
        | otherwise -> Left err /\ s1

instance Lazy (Parser a) where
  defer l = Parser \s -> let Parser k = l unit in k s

execParser :: forall a. Parser a -> ParserState -> ParseResult a
execParser p s0 = fst $ runParser p s0

runParser :: forall a. Parser a -> ParserState -> ParseResult a /\ ParserState
runParser (Parser k) s0 = k s0

initialState :: String -> ParserState
initialState src = { src, pos: SourcePos 1 1 }

failwith :: forall a. ParseError -> Parser a
failwith err = Parser \s -> Left err /\ s

optional :: forall a. Parser a -> Parser (Maybe a)
optional (Parser p) = Parser \s -> case p s of
  Right a /\ s1 -> Right (Just a) /\ s1
  Left e /\ s1
    | Right s' <- recover s1 e -> Right Nothing /\ s'
    | otherwise -> Left e /\ s1

many :: forall a. Parser a -> Parser (Array a)
many (Parser p) = Parser \s -> go [] s
  where
  go acc s0 = case p s0 of
    Right a /\ s1 -> go (acc <> [ a ]) s1
    Left e /\ s1
      | Right s1' <- recover s1 e -> Right acc /\ s1'
      | otherwise -> Left e /\ s1

try :: forall a. Parser a -> Parser a
try (Parser p) = Parser \s -> case p s of
  Right a /\ s1 -> Right a /\ s1
  Left e /\ _ -> Left e /\ s

followedBy :: forall a b. Parser a -> Parser b -> Parser (a /\ b)
followedBy p1 p2 = try ((/\) <$> p1 <*> p2)

delimBy :: forall a. Parser SourceToken -> Parser a -> Parser (Array a)
delimBy dlm p = do
  x <- optional p
  case x of
    Nothing -> pure []
    Just x0 -> do
      xs <- many (dlm `followedBy` p)
      pure $ Array.cons x0 (map snd xs)

tokenSuchThat :: (Token -> Boolean) -> Parser SourceToken
tokenSuchThat f = Parser \s -> case runLexer tokenize s of
  LexFail err src' -> Left { at: err.pos ~ err.pos, desc: err.desc, toks: [] } /\ { src: src', pos: err.pos }
  LexSuccess st src' pos'
    | f st.it -> Right st /\ { src: src', pos: pos' }
    | otherwise -> Left { at: st.at .. st.at, desc: UnexpectedToken st.it, toks: [ st ] } /\ s

token :: Token -> Parser SourceToken
token t = tokenSuchThat (_ == t)

-- tokens 
leftParens :: Parser SourceToken
leftParens = token TokLeftParens

rightParens :: Parser SourceToken
rightParens = token TokRightParens

leftSquare :: Parser SourceToken
leftSquare = token TokLeftSquare

rightSquare :: Parser SourceToken
rightSquare = token TokRightSquare

equal :: Parser SourceToken
equal = token TokEqual

dot :: Parser SourceToken
dot = token TokDot

comma :: Parser SourceToken
comma = token TokComma

colon :: Parser SourceToken
colon = token TokColon

pipe :: Parser SourceToken
pipe = token TokPipe

semicolon :: Parser SourceToken
semicolon = token TokSemicolon

underscore :: Parser SourceToken
underscore = token TokUnderscore

star :: Parser SourceToken
star = tokenSuchThat case _ of
  TokOperator "*" -> true
  _ -> false

rightArrow :: Parser SourceToken
rightArrow = token TokRightArrow

fn :: Parser SourceToken
fn = token (TokReserved KW_fn)

as :: Parser SourceToken
as = token (TokReserved KW_as)

let_ :: Parser SourceToken
let_ = token (TokReserved KW_let)

rec_ :: Parser SourceToken
rec_ = token (TokReserved KW_rec)

and_ :: Parser SourceToken
and_ = token (TokReserved KW_and)

in_ :: Parser SourceToken
in_ = token (TokReserved KW_in)

if_ :: Parser SourceToken
if_ = token (TokReserved KW_if)

then_ :: Parser SourceToken
then_ = token (TokReserved KW_then)

else_ :: Parser SourceToken
else_ = token (TokReserved KW_else)

match :: Parser SourceToken
match = token (TokReserved KW_match)

matchfn :: Parser SourceToken
matchfn = token (TokReserved KW_matchfn)

with :: Parser SourceToken
with = token (TokReserved KW_with)

def :: Parser SourceToken
def = token (TokReserved KW_def)

alias :: Parser SourceToken
alias = token (TokReserved KW_alias)

assoc :: Parser SourceToken
assoc = tokenSuchThat case _ of
  TokIdent (Ident "assoc") -> true
  _ -> false

left :: Parser SourceToken
left = tokenSuchThat case _ of
  TokIdent (Ident "left") -> true
  _ -> false

right :: Parser SourceToken
right = tokenSuchThat case _ of
  TokIdent (Ident "right") -> true
  _ -> false

none :: Parser SourceToken
none = tokenSuchThat case _ of
  TokIdent (Ident "none") -> true
  _ -> false

type_ :: Parser SourceToken
type_ = token (TokReserved KW_type)

true_ :: Parser SourceToken
true_ = tokenSuchThat case _ of
  TokBool _ true -> true
  _ -> false

false_ :: Parser SourceToken
false_ = tokenSuchThat case _ of
  TokBool _ false -> true
  _ -> false

unit_ :: Parser SourceToken
unit_ = tokenSuchThat case _ of
  TokUnit -> true
  _ -> false

anyToken :: Parser SourceToken
anyToken = tokenSuchThat (const true)

int :: Parser (SourcePhrase Int)
int = do
  tok <- tokInt
  pure case tok of
    { at, it: TokInt _ n } -> n @@ at
    _ -> unsafeCrashWith "Impossible"
  where
  tokInt :: Parser SourceToken
  tokInt = tokenSuchThat case _ of
    TokInt _ _ -> true
    _ -> false

ident :: Parser (SourcePhrase Ident)
ident = ident_ <#> mapPhrase case _ of
  TokIdent id -> id
  _ -> unsafeCrashWith "Impossible"

tokOperator :: Parser SourceToken
tokOperator = tokenSuchThat case _ of
  TokOperator _ -> true
  _ -> false

operator :: Parser (SourcePhrase OperatorName)
operator = tokOperator >>= case _ of
  { at: loc, it: TokOperator op } -> pure $ (OperatorName op) @@ loc
  _ -> unsafeCrashWith "Impossible"

ident_ :: Parser SourceToken
ident_ = tokenSuchThat case _ of
  TokIdent _ -> true
  _ -> false

parensed :: forall a. Parser (SourcePhrase a) -> Parser (SourcePhrase a)
parensed p = do
  tok1 <- leftParens
  a <- p
  tok2 <- rightParens
  pure $
    { at: tok1.at .. tok2.at, it: a.it }

parseExpr :: Parser (Expr Ann)
parseExpr = defer \_ ->
  do
    exp <- parseExpr1
    mbTyp <- optional do
      _ <- colon
      parseType
    case mbTyp of
      Nothing -> pure exp
      Just typ ->
        pure $ ExprTyped { loc: (exprAnn exp).loc .. (typeAnn typ).loc } exp typ

parseExpr1 :: Parser (Expr Ann)
parseExpr1 = defer \_ -> do
  exp <- parseExpr2
  opExps <- many do
    op /\ rhs <- operator `followedBy` parseExpr2
    pure { op, rhs }
  case Array.unsnoc opExps of
    Nothing -> pure exp
    Just { init, last } -> pure $ ExprOperator
      { loc: (exprAnn exp).loc .. (exprAnn last.rhs).loc }
      exp
      (NonEmptyArray.snoc' init last)

parseExpr2 :: Parser (Expr Ann)
parseExpr2 = defer \_ -> do
  hd <- parseExpr3
  tl <- many do
    _ /\ exp <- comma `followedBy` parseExpr3
    pure exp
  case Array.last tl of
    Nothing -> pure hd
    Just lst -> pure $
      ExprTuple
        { loc: (exprAnn hd).loc .. (exprAnn lst).loc }
        (Array.cons hd tl)

parseExpr3 :: Parser (Expr Ann)
parseExpr3 = defer \_ ->
  do
    exp <- parseExpr4
    args <- many parseExpr
    case Array.unsnoc args of
      Nothing -> pure exp
      Just { init, last } -> pure $
        ExprApp { loc: (exprAnn exp).loc .. (exprAnn last).loc }
          exp
          (NonEmptyArray.snoc' init last)

parseExpr4 :: Parser (Expr Ann)
parseExpr4 = defer \_ -> do
  parseExprFunc
    <|> parseExprIf
    <|> parseExprLet
    <|> parseExprMatch
    <|> parseExprMatchFn
    <|> parseExpr5

parseExprFunc :: Parser (Expr Ann)
parseExprFunc = defer \_ -> do
  { at: loc1 } <- fn
  arg <- parsePattern
  args <- many parsePattern
  _ <- rightArrow
  body <- parseExpr
  pure $ ExprFunc
    { loc: loc1 .. (exprAnn body).loc }
    (NonEmptyArray.cons' arg args)
    body

parseExprIf :: Parser (Expr Ann)
parseExprIf = defer \_ -> do
  { at: loc1 } <- if_
  cond <- parseExpr
  _ <- then_
  ifSo <- parseExpr
  { at: loc2 } <- else_
  notSo <- parseExpr
  pure $ ExprIf { loc: loc1 .. loc2 } cond ifSo notSo

parseExprLet :: Parser (Expr Ann)
parseExprLet = defer \_ -> do
  { at: loc1 } <- let_
  recursivity <-
    ( optional rec_
        <#> maybe NonRec (const Rec)
    )
  (({ it: binders } /\ _) /\ _) /\ body <- parseBinders
    `followedBy` (optional semicolon)
    `followedBy` in_
    `followedBy` parseExpr
  pure $ ExprLet { loc: loc1 } recursivity binders body

parseExprMatch :: Parser (Expr Ann)
parseExprMatch = defer \_ -> do
  ((({ at: loc1 } /\ heads) /\ _) /\ matrix) <- match
    `followedBy` matchHeads
    `followedBy` with
    `followedBy` parseMatchMatrix

  pure $
    ExprMatch
      { loc: loc1 .. loc1 }
      heads
      matrix
  where
  matchHeads = defer \_ -> do
    exp <- parseExpr5
    exps <- many (snd <$> comma `followedBy` parseExpr5)
    pure $ NonEmptyArray.cons' exp exps

parseMatchMatrix :: Parser (PatternMatrix Ann)
parseMatchMatrix = defer \_ -> do
  ln <- matchMatrixLine
  lines <- many matchMatrixLine
  pure $ PatternMatrix $ Array.cons ln lines
  where
  matchMatrixLine = defer \_ -> do
    (((_ /\ pats) /\ _) /\ act) <- pipe
      `followedBy` matchMatrixPatterns
      `followedBy` rightArrow
      `followedBy` parseExpr
    pure
      { pats
      , act
      }
  matchMatrixPatterns = defer \_ -> do
    pat <- parsePattern
    pats <- many (snd <$> comma `followedBy` parsePattern)
    pure $ Array.cons pat pats

parseExprMatchFn :: Parser (Expr Ann)
parseExprMatchFn = defer \_ -> do
  { at: loc1 } <- matchfn
  pats <- parsePatterns1
  matrix <- parseMatchMatrix
  pure $ ExprMatchFn { loc: loc1 .. loc1 }
    pats
    matrix
  where
  parsePatterns1 = defer \_ -> do
    pat <- parsePattern
    many parsePattern >>= case _ of
      [] -> pure $ NonEmptyArray.singleton pat
      pats -> pure $ NonEmptyArray.cons' pat pats

parseBinders :: Parser (SourcePhrase (NonEmptyArray (Binder Ann)))
parseBinders = defer \_ -> do
  hd <- parseBinder
  tl <- many (snd <$> semicolon `followedBy` parseBinder)
  case Array.last tl of
    Nothing -> pure
      { it: NonEmptyArray.singleton hd
      , at: (binderAnn hd).loc
      }
    Just b -> pure
      { it: NonEmptyArray.cons' hd tl
      , at: (binderAnn hd).loc .. (binderAnn b).loc
      }

parseBinder :: Parser (Binder Ann)
parseBinder = defer \_ -> do
  ((pat /\ _) /\ exp) <- parsePattern
    `followedBy` equal
    `followedBy` parseExpr
  pure $ Binder
    { loc: (patternAnn pat).loc .. (exprAnn exp).loc }
    pat
    exp

parseExpr5 :: Parser (Expr Ann)
parseExpr5 = defer \_ ->
  do
    parseExprList
    <|> parseExprAtom

parseExprAtom :: Parser (Expr Ann)
parseExprAtom = defer \_ ->
  do
    parseExprConst
    <|> parseExprIdent
    <|> parseParensExpr

parseExprIdent :: Parser (Expr Ann)
parseExprIdent = defer \_ -> do
  ident <#> \{ at, it } -> ExprIdent { loc: at } Nothing it

parseExprList :: Parser (Expr Ann)
parseExprList = defer \_ -> do
  { at: loc1 } <- leftSquare
  exps <- do
    exps <- many (fst <$> parseExpr `followedBy` semicolon)
    mbExp <- optional parseExpr
    case mbExp of
      Nothing -> pure exps
      Just exp -> pure $ Array.snoc exps exp
  { at: loc2 } <- rightSquare
  pure $ ExprList { loc: loc1 .. loc2 } exps

parseConstructorName :: Parser (SourcePhrase ConstructorName)
parseConstructorName = defer \_ -> do
  ident_ >>= case _ of
    st@{ at, it: tok }
      | TokIdent (Ident name) <- tok ->
          if constrRegex `Re.test` name then
            pure { at, it: ConstructorName name }
          else
            failwith ({ at, toks: [ st ], desc: NotAConstructorName name })
      | otherwise -> unsafeCrashWith "Impossible"
  where
  constrRegex = unsafeRegex """[A-Z][a-zA-Z0-9'_]*""" unicode

parseExprConst :: Parser (Expr Ann)
parseExprConst = defer \_ -> do
  parseAtomicConstant <#> \{ at, it } -> ExprConst { loc: at } it

parseParensExpr :: Parser (Expr Ann)
parseParensExpr = defer \_ -> do
  { at: loc1 } <- leftParens
  exp <- parseExpr
  { at: loc2 } <- rightParens
  pure $ ExprParensed { loc: loc1 .. loc2 } exp

parsePattern :: Parser (Pattern Ann)
parsePattern = defer \_ -> do
  pat <- parsePattern1
  mbIdent <- optional do
    _ <- as
    ident
  case mbIdent of
    Nothing -> pure pat
    Just { at: loc2, it: alias } ->
      pure $ PatAlias { loc: (patternAnn pat).loc .. loc2 } pat alias

parsePattern1 :: Parser (Pattern Ann)
parsePattern1 = defer \_ ->
  do
    pat <- parsePattern2
    mbTyp <- optional do
      _ <- colon
      parseType
    case mbTyp of
      Nothing -> pure pat
      Just typ -> pure $
        PatTyped { loc: (patternAnn pat).loc .. (typeAnn typ).loc } pat typ

parsePattern2 :: Parser (Pattern Ann)
parsePattern2 = defer \_ ->
  do
    parsePatConstructor
    <|> parsePatternAtom

parsePatConstructor :: Parser (Pattern Ann)
parsePatConstructor = defer \_ -> do
  { at: loc1, it: constr } <- ident
  pats <- many parsePatternAtom
  case Array.last pats of
    Nothing
      | isConstructorName constr -> pure $ PatConstructor { loc: loc1 } constr []
      | otherwise -> pure $ PatVar { loc: loc1 } constr
    Just pat -> pure $ PatConstructor { loc: loc1 .. (patternAnn pat).loc } constr pats

parsePatternAtom :: Parser (Pattern Ann)
parsePatternAtom = defer \_ ->
  do
    parsePatWildcard
    <|> parsePatVar
    <|> parsePatConst
    <|> parsePatParensed

parsePatParensed :: Parser (Pattern Ann)
parsePatParensed = defer \_ -> do
  { at: loc1 } <- leftParens
  pat <- parsePattern
  { at: loc2 } <- rightParens
  pure $ PatParensed { loc: loc1 .. loc2 } pat

parsePatWildcard :: Parser (Pattern Ann)
parsePatWildcard = defer \_ -> do
  underscore <#> \{ at: loc } -> PatWildcard { loc }

parsePatVar :: Parser (Pattern Ann)
parsePatVar = defer \_ -> do
  ident <#> \{ at: loc, it } -> PatVar { loc } it

parsePatConst :: Parser (Pattern Ann)
parsePatConst = defer \_ -> do
  parseAtomicConstant <#> \{ at: loc, it: cst } -> PatConst { loc } cst

parseAtomicConstant ∷ Parser (SourcePhrase AtomicConstant)
parseAtomicConstant = defer \_ ->
  do
    parseConstBool
    <|> parseConstInt
    <|> parseConstUnit

parseConstBool :: Parser (SourcePhrase AtomicConstant)
parseConstBool = defer \_ ->
  do
    (true_ <#> \{ at } -> ACBool true @@ at)
    <|> (false_ <#> \{ at } -> ACBool false @@ at)

parseConstInt :: Parser (SourcePhrase AtomicConstant)
parseConstInt = defer \_ -> do
  int <#> mapPhrase ACInt

parseConstUnit :: Parser (SourcePhrase AtomicConstant)
parseConstUnit = defer \_ -> do
  unit_ <#> mapPhrase (const ACUnit)

parseType :: Parser (Type_ Ann)
parseType = defer \_ -> do
  typ <- parseType1
  typs <- many (rightArrow *> parseType1)
  case NonEmptyArray.fromArray typs of
    Nothing -> pure typ
    Just ts -> do
      let { init, last } = NonEmptyArray.unsnoc ts
      pure $
        TFunc
          { loc: (typeAnn typ).loc .. (typeAnn last).loc }
          (NonEmptyArray.cons' typ init)
          last

parseType1 :: Parser (Type_ Ann)
parseType1 = defer \_ -> do
  t1 <- parseTypeAtom
  ts <- many (star *> parseTypeAtom)
  case ts of
    [] -> pure t1
    _ ->
      let
        last = unsafePartial (ArrayP.last ts)
      in
        pure $ TTup
          { loc: (typeAnn t1).loc .. (typeAnn last).loc }
          (Array.cons t1 ts)

parseTypeAtom :: Parser (Type_ Ann)
parseTypeAtom = defer \_ ->
  do
    parseTypeFree
    <|> parseParensType

parseTypeFree :: Parser (Type_ Ann)
parseTypeFree = defer \_ -> do
  name <- ident
  pure $ TFree { loc: name.at } name.it

parseParensType :: Parser (Type_ Ann)
parseParensType = defer \_ -> do
  { at: lc1 } <- leftParens
  typ <- parseType
  { at: lc2 } <- rightParens
  pure $ TParens
    { loc: lc1 .. lc2 }
    typ

parseDeclaration :: Parser (Declaration Ann)
parseDeclaration = defer \_ -> do
  parseDeclarationDeclRec
    <|> parseDeclarationDecl
    <|> parseDeclarationAlias

parseDeclarationDecl :: Parser (Declaration Ann)
parseDeclarationDecl = defer \_ -> do
  { at: loc1 } <- let_
  { name, pats, exp } <- parseDeclarationBinder
  { at: loc2 } <- semicolon
  pure $ Decl
    { loc: loc1 .. loc2 }
    name
    pats
    exp

parseDeclarationDeclRec :: Parser (Declaration Ann)
parseDeclarationDeclRec = defer \_ -> do
  { at: loc1 } <- fst <$> (let_ `followedBy` rec_)
  binder <- parseDeclarationBinder
  binders <- many do
    (_ /\ b) <- and_ `followedBy` parseDeclarationBinder
    pure b
  { at: loc2 } <- semicolon
  pure $
    DeclRec
      { loc: loc1 .. loc2 }
      (Array.cons binder binders)

parseDeclarationBinder :: Parser { name :: SourcePhrase Ident, pats :: Array (Pattern Ann), exp :: Expr Ann }
parseDeclarationBinder = defer \_ -> do
  ((name /\ pats) /\ _) /\ exp <- ident
    `followedBy` (many parsePattern)
    `followedBy` equal
    `followedBy` parseExpr
  pure
    { name
    , pats
    , exp
    }

parsePatterns :: Parser (Pattern Ann) -> Parser (SourcePhrase (NonEmptyArray (Pattern Ann)))
parsePatterns p = defer \_ -> do
  pat <- parsePattern
  pats <- many p
  case Array.last pats of
    Nothing -> pure (NonEmptyArray.singleton pat @@ (patternAnn pat).loc)
    Just last ->
      let
        loc = (patternAnn pat).loc .. (patternAnn last).loc
      in
        pure $ NonEmptyArray.cons' pat pats @@ loc

parseDeclarationAlias :: Parser (Declaration Ann)
parseDeclarationAlias = defer \_ -> do
  ((({ at: loc1 } /\ _) /\ name) /\ _) /\ opname <- def
    `followedBy` alias
    `followedBy` ident
    `followedBy` as
    `followedBy` operator
  _ <- leftParens
  a <- parseAssociativity
  _ <- comma
  precedence <- int
  { at: loc2 } <- rightParens

  pure $
    DeclAlias { loc: loc1 .. loc2 }
      name
      opname
      a
      precedence
  where
  parseAssociativity = do
    _ /\ a <- assoc `followedBy` (left <|> right <|> none)
    pure $ a # mapPhrase case _ of
      TokIdent (Ident id) -> case id of
        "left" -> LeftAssoc
        "right" -> RightAssoc
        "none" -> NonAssoc
        _ -> unsafeCrashWith "Impossible"
      _ -> unsafeCrashWith "Impossible"

parse :: String -> String -> Either ParseError (Module Ann)
parse name src = tailRec go ([] /\ initialState src)
  where
  go (decls /\ st) =
    case runParser parseDeclaration st of
      Right decl /\ st' -> Loop $ (Array.snoc decls decl) /\ st'
      Left err /\ { pos: eof }
        | UnexpectedToken TokEOF <- err.desc
        , [ { it: TokEOF } ] <- err.toks ->
            Done $ Right $ Module
              { loc: SourcePos 1 1 ~ eof
              , name: wrap name
              , decls
              }
        | otherwise -> Done $ Left err