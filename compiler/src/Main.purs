module Main where

import Prelude

import DAM4G.Compiler.Backend.Lower (lower)
import DAM4G.Compiler.Name (Ident(..), ModuleName(..))
import DAM4G.Compiler.Optimizer.IR (Decl(..), ELC(..), Primitive(..), Var(..))
import DAM4G.Compiler.Optimizer.IR as IR
import DAM4G.Compiler.Value (Constant(..))
import Effect (Effect)
import Effect.Class.Console as Console

sample :: IR.Module
sample = IR.Module
  { name: ModuleName "Sample"
  , decls:
    [ IR.NonRec $ Decl 
      { ident: Ident "add"
      , term: ELAbs unit 2 
          ( ELPrim unit P_i32_add
            [ ELVar unit (Var 1)
            , ELVar unit (Var 0)
            ]
          )
      }
    , IR.NonRec $ Decl
        { ident: Ident "succ"
        , term: ELAbs unit 1
          ( ELApp unit 
            (ELPrim unit (PGetGlobal $ Ident "add") [])
            [ ELVar unit (Var 0) 
            , ELConst unit (CstInt 1)
            ]
          )
        }
    , IR.NonRec $ Decl
      { ident: Ident "it"
      , term: 
          ELApp unit 
            (ELPrim unit (PGetGlobal $ Ident "succ") [])
            [ ELConst unit (CstInt 42)
            ]
      }
    ]
  }

main :: Effect Unit
main = do
  Console.logShow $ lower sample