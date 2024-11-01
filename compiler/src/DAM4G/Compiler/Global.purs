module DAM4G.Compiler.Global where

import Prelude

import DAM4G.Compiler.Name (ConstructorName(..), GlobalName, Ident, OperatorName, Qualified, unqualify)
import DAM4G.Compiler.Types (Associativity, Type_)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\))

data GlobalDesc
  = Normal
  | Constructor ConstructorDesc
  | Prim PrimDesc

type ConstructorArg =
  { name :: Ident
  , typ :: Type_ Unit
  }

type ConstructorDesc =
  { name :: Qualified ConstructorName
  , typname :: GlobalName
  , arity :: Int
  , args :: Array ConstructorArg
  }

type PrimDesc = {}

type GlobalInfo =
  { desc :: GlobalDesc
  , opened :: Boolean
  }

type OperatorInfo =
  { realname :: GlobalName
  , assoc :: Associativity
  , prec :: Int
  }

type Env =
  { globals :: Map.Map GlobalName GlobalInfo
  , aliases :: Map.Map (Qualified OperatorName) OperatorInfo
  }

emptyEnv :: Env
emptyEnv =
  { globals: Map.empty
  , aliases: Map.empty
  }

insertOperatorAlias :: Qualified OperatorName -> OperatorInfo -> Env -> Env
insertOperatorAlias opname op env = env
  { aliases = env.aliases # Map.insert opname op }

lookupOpenedName :: Ident -> Env -> Maybe (GlobalName /\ GlobalDesc)
lookupOpenedName ident env = env.globals
  # Map.filter _.opened
  # Map.toUnfoldable
  # Array.find (fst >>> unqualify >>> (_ == ident))
  <#> rmap _.desc
