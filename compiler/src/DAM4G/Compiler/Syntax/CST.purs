module DAM4G.Compiler.Syntax.CST where

import Prelude

import DAM4G.Compiler.Constant (AtomicConstant)
import DAM4G.Compiler.Name (Ident, ModuleName(..), OperatorName(..))
import DAM4G.Compiler.Syntax.Source (SourceLoc(..))
import DAM4G.Compiler.Syntax.Source as Source
import DAM4G.Compiler.Types (Associativity(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Fmt as Fmt

type SourcePhrase a = Source.SourcePhrase () a
data Token
  = TokLeftParens
  | TokRightParens
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokDot
  | TokComma
  | TokEqual
  | TokColon
  | TokSemicolon
  | TokUnderscore
  | TokPipe
  | TokRightArrow
  | TokReserved Keyword
  | TokOperator String
  | TokIdent Ident
  | TokUnit
  | TokInt String Int
  | TokBool String Boolean
  | TokString String String
  | TokEOF

derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token
derive instance genericToken :: Generic Token _
instance showToken :: Show Token where
  show = genericShow

data Keyword
  = KW_if
  | KW_then
  | KW_else
  | KW_let
  | KW_rec
  | KW_and
  | KW_in
  | KW_fn
  | KW_match
  | KW_with
  | KW_matchfn
  | KW_def
  | KW_type
  | KW_alias
  | KW_as

derive instance eqKeyword :: Eq Keyword
derive instance ordKeyword :: Ord Keyword
derive instance genericKeyword :: Generic Keyword _
instance showKeyword :: Show Keyword where
  show = genericShow

type SourceToken = SourcePhrase Token

data Recursivity = Rec | NonRec

derive instance Generic Recursivity _
derive instance Eq Recursivity
instance Show Recursivity where
  show = genericShow

data Expr a
  = ExprConst a AtomicConstant
  | ExprIdent a Ident
  | ExprList a (Array (Expr a))
  | ExprTuple a (Array (Expr a))
  | ExprApp a (Expr a) (NonEmptyArray (Expr a))
  | ExprOperator a (Expr a) (NonEmptyArray { op :: SourcePhrase OperatorName, rhs :: Expr a })
  | ExprFunc a (NonEmptyArray (Pattern a)) (Expr a)
  | ExprLet a Recursivity (NonEmptyArray (Binder a)) (Expr a)
  | ExprConstructor a Ident (Array (Expr a))
  | ExprIf a (Expr a) (Expr a) (Expr a)
  | ExprMatch a (NonEmptyArray (Expr a)) (PatternMatrix a)
  | ExprMatchFn a (NonEmptyArray (Pattern a)) (PatternMatrix a)
  | ExprTyped a (Expr a) (Type_ a)
  | ExprParensed a (Expr a)

derive instance Functor Expr
derive instance Eq a => Eq (Expr a)
derive instance Generic (Expr a) _
instance Show a => Show (Expr a) where
  show it = genericShow it

data Binder a = Binder a (Pattern a) (Expr a)

derive instance Functor Binder
derive instance Eq a => Eq (Binder a)
derive instance Generic (Binder a) _
instance Show a => Show (Binder a) where
  show it = genericShow it

newtype PatternMatrix a = PatternMatrix
  ( Array
      { pats :: Array (Pattern a)
      , act :: Expr a
      }
  )

derive instance Functor PatternMatrix
derive instance Newtype (PatternMatrix a) _
derive instance Eq a => Eq (PatternMatrix a)
instance Show a => Show (PatternMatrix a) where
  show (PatternMatrix pm) = Fmt.fmt @"(PatternMatric {pm})" { pm: show pm }

data Pattern a
  = PatWildcard a
  | PatVar a Ident
  | PatConst a AtomicConstant
  | PatList a (Array (Pattern a))
  | PatConstructor a Ident (Array (Pattern a))
  | PatAlias a (Pattern a) Ident
  | PatParensed a (Pattern a)
  | PatTyped a (Pattern a) (Type_ a)

derive instance Functor Pattern
derive instance Generic (Pattern a) _
derive instance Eq a => Eq (Pattern a)
instance Show a => Show (Pattern a) where
  show it = genericShow it

data Type_ a
  = TFunc a (NonEmptyArray (Type_ a)) (Type_ a)
  | TFree a Ident
  | TTup a (Array (Type_ a))
  | TParens a (Type_ a)

derive instance functorType :: Functor Type_
derive instance eqType :: Eq a => Eq (Type_ a)
derive instance genericType :: Generic (Type_ a) _
instance showType :: Show a => Show (Type_ a) where
  show it = genericShow it

type Ann =
  { loc :: SourceLoc
  }

typeAnn :: forall a. Type_ a -> a
typeAnn = case _ of
  TFunc a _ _ -> a
  TFree a _ -> a
  TTup a _ -> a
  TParens a _ -> a

exprAnn :: forall a. Expr a -> a
exprAnn = case _ of
  ExprConst a _ -> a
  ExprList a _ -> a
  ExprTuple a _ -> a
  ExprIdent a _ -> a
  ExprFunc a _ _ -> a
  ExprApp a _ _ -> a
  ExprOperator a _ _ -> a
  ExprLet a _ _ _ -> a
  ExprConstructor a _ _ -> a
  ExprIf a _ _ _ -> a
  ExprMatch a _ _ -> a
  ExprMatchFn a _ _ -> a
  ExprTyped a _ _ -> a
  ExprParensed a _ -> a

patternAnn :: forall a. Pattern a -> a
patternAnn = case _ of
  PatWildcard a -> a
  PatVar a _ -> a
  PatConst a _ -> a
  PatList a _ -> a
  PatConstructor a _ _ -> a
  PatAlias a _ _ -> a
  PatParensed a _ -> a
  PatTyped a _ _ -> a

binderAnn :: forall a. Binder a -> a
binderAnn (Binder a _ _) = a

data Declaration a
  = Decl a (SourcePhrase Ident) (Array (Pattern a)) (Expr a)
  | DeclRec a (Array { name :: SourcePhrase Ident, pats :: Array (Pattern a), exp :: Expr a })
  | DeclAlias a (SourcePhrase Ident) (SourcePhrase OperatorName) (SourcePhrase Associativity) (SourcePhrase Int)

-- | DeclSet a (SourcePhrase Ident) ()

derive instance Functor Declaration
derive instance Generic (Declaration a) _
derive instance Eq a => Eq (Declaration a)
instance Show a => Show (Declaration a) where
  show = genericShow

newtype Module a = Module
  { name :: ModuleName
  , loc :: SourceLoc
  , decls :: Array (Declaration a)
  }

derive instance Newtype (Module a) _
derive instance Eq a => Eq (Module a)
instance Show a => Show (Module a) where
  show (Module m) = Fmt.fmt @"(Module {m})" { m: show m }