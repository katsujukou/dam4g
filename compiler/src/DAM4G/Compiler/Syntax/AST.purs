module DAM4G.Compiler.Syntax.AST where

import Prelude

import DAM4G.Compiler.Constant (AtomicConstant)
import DAM4G.Compiler.Name (ConstructorName, GlobalName, Ident, ModuleName, OperatorName, Qualified)
import DAM4G.Compiler.Syntax.Source (SourceLoc, SourcePhrase, emptyLoc)
import DAM4G.Compiler.Types as T
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Fmt as Fmt

type SourceIdent = SourcePhrase () Ident

data Expr a
  = ExprConst a AtomicConstant
  | ExprVar a Ident
  | ExprGlobal a GlobalName
  | ExprFunc a SourceIdent (Maybe Type_) (Expr a)
  | ExprApp a (Expr a) (Expr a)
  | ExprIf a (Expr a) (Expr a) (Expr a)
  | ExprMatch a (Array (Expr a)) (MatchMatrix a)
  | ExprTyped a (Expr a) Type_

derive instance Functor Expr
derive instance Foldable Expr
derive instance Traversable Expr
derive instance Eq a => Eq (Expr a)
derive instance Generic (Expr a) _
instance Show a => Show (Expr a) where
  show it = genericShow it

data Pattern a
  = PatWildcard a
  | PatVar a Ident
  | PatConst a AtomicConstant
  | PatConstructor a (Qualified ConstructorName) (Array (Pattern a))
  | PatAlias a (Pattern a) Ident
  | PatTyped a (Pattern a) Type_

derive instance Functor Pattern
derive instance Foldable Pattern
derive instance Traversable Pattern
derive instance Generic (Pattern a) _
derive instance Eq a => Eq (Pattern a)
instance Show a => Show (Pattern a) where
  show it = genericShow it

newtype MatchMatrix a = MatchMatrix
  ( Array
      { pats :: Array (Pattern a)
      , act :: Expr a
      }
  )

derive instance Newtype (MatchMatrix a) _
derive instance Functor MatchMatrix
derive instance Foldable MatchMatrix
derive instance Traversable MatchMatrix
derive instance Eq a => Eq (MatchMatrix a)
instance Show a => Show (MatchMatrix a) where
  show (MatchMatrix m) = Fmt.fmt @"(MatchMatrix {m})" { m: show m }

data NameSource
  = User SourceLoc
  | Compiler

derive instance Eq NameSource
derive instance Generic NameSource _
instance Show NameSource where
  show = genericShow

type Type_ = T.Type_ Ann

data Ann
  = AnnExpr SourceLoc (T.Type_ Ann)
  | AnnType NameSource

derive instance Eq Ann
derive instance Generic Ann _
instance Show Ann where
  show it = genericShow it

annLoc :: Ann -> Maybe SourceLoc
annLoc = case _ of
  AnnExpr loc _ -> Just loc
  AnnType src
    | User loc <- src -> Just loc
    | otherwise -> Nothing

annLoc' :: Ann -> SourceLoc
annLoc' = annLoc >>> fromMaybe emptyLoc

data Declaration a
  = NonRec { ident :: SourceIdent, expr :: Expr a }
  | Rec (Array { ident :: SourceIdent, expr :: Expr a })

derive instance Eq a => Eq (Declaration a)
derive instance Generic (Declaration a) _
instance Show a => Show (Declaration a) where
  show it = genericShow it

newtype OperatorInfo = OperatorInfo
  { loc :: SourceLoc
  , opname :: Qualified OperatorName
  , assoc :: T.Associativity
  , prec :: Int
  , realname :: GlobalName
  }

derive instance Eq OperatorInfo
derive instance Newtype OperatorInfo _
instance Show OperatorInfo where
  show (OperatorInfo o) = Fmt.fmt @"(OperatorInfo {o})" { o: show o }

newtype Module a = Module
  { name :: ModuleName
  , loc :: SourceLoc
  , decls :: Array (Declaration a)
  , operators :: Array OperatorInfo
  }

derive instance Newtype (Module a) _
instance Show a => Show (Module a) where
  show (Module m) = Fmt.fmt @"(Module {m})" { m: show m }

exprAnn :: forall a. Expr a -> a
exprAnn = case _ of
  ExprConst a _ -> a
  ExprVar a _ -> a
  ExprGlobal a _ -> a
  ExprFunc a _ _ _ -> a
  ExprApp a _ _ -> a
  ExprIf a _ _ _ -> a
  ExprMatch a _ _ -> a
  ExprTyped a _ _ -> a

