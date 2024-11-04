module DAM4G.Compiler.Global where

import Prelude

import DAM4G.Compiler.Name (ConstructorName, GlobalName, Ident, OperatorName, Qualified(..), TypeName, unqualify)
import DAM4G.Compiler.Primitive (Primitive)
import DAM4G.Compiler.Types (Associativity)
import DAM4G.Compiler.Types as T
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

type Type_ = T.Type_ T.TypeAnn

data GlobalDesc
  = Normal NormalDesc
  | Constructor ConstructorDesc
  | Prim PrimDesc

derive instance Eq GlobalDesc
derive instance Generic GlobalDesc _
instance Show GlobalDesc where
  show = genericShow

type NormalDesc = {}

type ConstructorDesc =
  { name :: Qualified ConstructorName
  , typname :: Qualified TypeName
  , sig :: T.Type_ Unit
  }

type PrimDesc =
  { prim :: Primitive }

type TypeInfo =
  { kind :: T.Kind Unit
  , constrs :: Array GlobalName
  , opened :: Boolean
  }

type GlobalInfo =
  { desc :: GlobalDesc
  , typ :: T.Type_ Unit
  , opened :: Boolean
  }

type OperatorInfo =
  { realname :: GlobalName
  , assoc :: Associativity
  , prec :: Int
  , opened :: Boolean
  }

type Env =
  { types :: Map.Map (Qualified TypeName) TypeInfo
  , globals :: Map.Map GlobalName GlobalInfo
  , aliases :: Map.Map (Qualified OperatorName) OperatorInfo
  }

emptyEnv :: Env
emptyEnv =
  { types: Map.empty
  , globals: Map.empty
  , aliases: Map.empty
  }

insertDecl :: GlobalName -> GlobalInfo -> Env -> Env
insertDecl gloname info env = env
  { globals = Map.insert gloname info env.globals }

insertType :: Qualified TypeName -> TypeInfo -> Env -> Env
insertType typename typeInfo env = env
  { types = env.types # Map.insert typename typeInfo }

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

listOpenedDecls :: Env -> Array (GlobalName /\ GlobalInfo)
listOpenedDecls { globals } =
  globals
    # Map.filter _.opened
    # Map.toUnfoldable

listOpenedType :: Env -> Array (Qualified TypeName /\ TypeInfo)
listOpenedType { types } =
  types
    # Map.filter _.opened
    # Map.toUnfoldable