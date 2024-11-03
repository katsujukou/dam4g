module DAM4G.Compiler.Global where

import Prelude

import DAM4G.Compiler.Name (ConstructorName, GlobalName, Ident, OperatorName(..), Qualified(..), unqualify)
import DAM4G.Compiler.Primitive (Primitive)
import DAM4G.Compiler.Types (Associativity, Type_)
import DAM4G.Compiler.Types as T
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

data GlobalDesc
  = Normal NormalDesc
  | Constructor ConstructorDesc
  | Prim PrimDesc

derive instance Eq GlobalDesc
derive instance Generic GlobalDesc _
instance Show GlobalDesc where
  show = genericShow

type NormalDesc = {}

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

type PrimDesc =
  { prim :: Primitive }

type TypeInfo =
  { opened :: Boolean
  }

type GlobalInfo =
  { desc :: GlobalDesc
  , typ :: Type_ Unit
  , opened :: Boolean
  }

type OperatorInfo =
  { realname :: GlobalName
  , assoc :: Associativity
  , prec :: Int
  , opened :: Boolean
  }

type Env =
  { types :: Map.Map GlobalName TypeInfo
  , globals :: Map.Map GlobalName GlobalInfo
  , aliases :: Map.Map (Qualified OperatorName) OperatorInfo
  }

emptyEnv :: Env
emptyEnv =
  { types: Map.empty
  , globals: Map.empty
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

lookupOpenedAlias :: Env -> OperatorName -> Maybe OperatorInfo
lookupOpenedAlias { aliases } opname' =
  aliases
    # Map.toUnfoldable
    # Array.find
        (\((Qualified _ opname) /\ { opened }) -> opened && opname == opname')
    <#> snd

listOpenedType :: Env -> Array (Ident /\ (GlobalName /\ TypeInfo))
listOpenedType { types } =
  types
    # Map.filter _.opened
    # Map.toUnfoldable
    <#> \(gloname@(Qualified _ ident) /\ typ) -> ident /\ gloname /\ typ