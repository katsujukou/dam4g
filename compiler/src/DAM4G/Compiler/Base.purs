module DAM4G.Compiler.Base where

import Prelude

import DAM4G.Compiler.Global (OperatorInfo)
import DAM4G.Compiler.Global as G
import DAM4G.Compiler.Name (Ident(..), ModuleName(..), OperatorName(..), Qualified(..), GlobalName)
import DAM4G.Compiler.Primitive (Primitive(..))
import DAM4G.Compiler.Types (Associativity(..))
import DAM4G.Compiler.Types as T
import Data.Map as Map
import Data.Tuple.Nested ((/\))

base :: ModuleName
base = ModuleName "Base"

baseIdent :: String -> GlobalName
baseIdent = Ident >>> Qualified base

baseOperator :: String -> Qualified OperatorName
baseOperator op = Qualified base (OperatorName op)

operatorPlus :: Qualified OperatorName
operatorPlus = baseOperator "+"

operatorStar :: Qualified OperatorName
operatorStar = baseOperator "*"

operatorSlash :: Qualified OperatorName
operatorSlash = baseOperator "/"

operatorDash :: Qualified OperatorName
operatorDash = baseOperator "-"

operatorEqualEqual :: Qualified OperatorName
operatorEqualEqual = baseOperator "=="

operatorSlashEqual :: Qualified OperatorName
operatorSlashEqual = baseOperator "/="

operatorLAngle :: Qualified OperatorName
operatorLAngle = baseOperator "<"

operatorRAngle :: Qualified OperatorName
operatorRAngle = baseOperator ">"

operatorLAngleEqual :: Qualified OperatorName
operatorLAngleEqual = baseOperator "<="

operatorRAngleEqual :: Qualified OperatorName
operatorRAngleEqual = baseOperator ">="

intAdd :: GlobalName
intAdd = baseIdent "intAdd"

intMul :: GlobalName
intMul = baseIdent "intMul"

intSub :: GlobalName
intSub = baseIdent "intSub"

intDiv :: GlobalName
intDiv = baseIdent "intDiv"

intEqu :: GlobalName
intEqu = baseIdent "intEqu"

intNeq :: GlobalName
intNeq = baseIdent "intNeq"

intLE :: GlobalName
intLE = baseIdent "intLE"

intLT :: GlobalName
intLT = baseIdent "intLT"

intGE :: GlobalName
intGE = baseIdent "intGE"

intGT :: GlobalName
intGT = baseIdent "intGT"

int :: GlobalName
int = baseIdent "Int"

bool :: GlobalName
bool = baseIdent "Bool"

typInt :: T.Type_ Unit
typInt = T.TGlobal unit int

typBool :: T.Type_ Unit
typBool = T.TGlobal unit bool

mkArrowType :: T.Type_ Unit -> T.Type_ Unit -> T.Type_ Unit
mkArrowType t1 t2 = T.TFunc unit t1 t2

infixr 5 mkArrowType as .->

types :: Map.Map GlobalName G.TypeInfo
types = Map.fromFoldable
  [ int /\ { opened: true }
  , bool /\ { opened: true }
  ]

globals :: Map.Map GlobalName G.GlobalInfo
globals = Map.fromFoldable
  [ intAdd /\
      { desc: G.Prim { prim: P_i32_add }
      , typ: typInt .-> typInt .-> typInt
      , opened: true
      }
  , intMul /\
      { desc: G.Prim { prim: P_i32_mul }
      , typ: typInt .-> typInt .-> typInt
      , opened: true
      }
  , intSub /\
      { desc: G.Prim { prim: P_i32_sub }
      , typ: typInt .-> typInt .-> typInt
      , opened: true
      }
  , intDiv /\
      { desc: G.Prim { prim: P_i32_div }
      , typ: typInt .-> typInt .-> typInt
      , opened: true
      }
  , intEqu /\
      { desc: G.Prim { prim: P_i32_equ }
      , typ: typInt .-> typInt .-> typBool
      , opened: true
      }
  , intNeq /\
      { desc: G.Prim { prim: P_i32_neq }
      , typ: typInt .-> typInt .-> typBool
      , opened: true
      }
  , intLE /\
      { desc: G.Prim { prim: P_i32_le }
      , typ: typInt .-> typInt .-> typBool
      , opened: true
      }
  , intLT /\
      { desc: G.Prim { prim: P_i32_lt }
      , typ: typInt .-> typInt .-> typBool
      , opened: true
      }
  , intGE /\
      { desc: G.Normal {}
      , typ: typInt .-> typInt .-> typBool
      , opened: true
      }
  , intGT /\
      { desc: G.Normal {}
      , typ: typInt .-> typInt .-> typBool
      , opened: true
      }
  ]

aliases :: Map.Map (Qualified OperatorName) OperatorInfo
aliases = Map.fromFoldable
  [ operatorPlus /\ { realname: intAdd, assoc: LeftAssoc, prec: 6, opened: true }
  , operatorStar /\ { realname: intMul, assoc: LeftAssoc, prec: 7, opened: true }
  , operatorSlash /\ { realname: intDiv, assoc: LeftAssoc, prec: 7, opened: true }
  , operatorDash /\ { realname: intSub, assoc: LeftAssoc, prec: 6, opened: true }
  , operatorEqualEqual /\ { realname: intEqu, assoc: NonAssoc, prec: 4, opened: true }
  , operatorSlashEqual /\ { realname: intNeq, assoc: NonAssoc, prec: 4, opened: true }
  , operatorLAngle /\ { realname: intLT, assoc: LeftAssoc, prec: 4, opened: true }
  , operatorLAngleEqual /\ { realname: intLE, assoc: LeftAssoc, prec: 4, opened: true }
  , operatorRAngle /\ { realname: intGT, assoc: LeftAssoc, prec: 4, opened: true }
  , operatorRAngleEqual /\ { realname: intGE, assoc: LeftAssoc, prec: 4, opened: true }
  ]

baseEnv :: G.Env
baseEnv =
  { types
  , globals
  , aliases
  }