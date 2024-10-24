module Main where

import Prelude

import DAM4G.Compiler.Backend.CodeGen (emit)
import DAM4G.Compiler.Backend.CodeGen as CodeGen
import DAM4G.Compiler.Backend.CodeGen.ObjectFile (Bytecode(..))
import DAM4G.Compiler.Backend.Lower (lower)
import DAM4G.Compiler.Name (Ident(..), ModuleName(..))
import DAM4G.Compiler.Optimizer.IR (Decl(..), ELC(..), Primitive(..), Var(..))
import DAM4G.Compiler.Optimizer.IR as IR
import DAM4G.Compiler.Value (Constant(..))
import Data.Int (hexadecimal, toStringAs)
import Data.String.CodeUnits (slice)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Buffer as Buffer
import Node.FS.Aff (writeFile)

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
      -- let f = fun x -> 
      --   let 
      --     prev = fun a -> sub a 1
      --     succ = add 1 
      --   in
      --     if x > 0 then succ x 
      --     else prev x
      , IR.NonRec $ Decl
          { ident: Ident "f"
          , term: ELAbs unit 1
              ( ELLet unit
                  [ ELAbs unit 1
                      ( ELPrim unit P_i32_sub
                          [ ELVar unit (Var 0)
                          , ELConst unit (CstInt 1)
                          ]
                      )
                  , ( ELApp unit
                        (ELPrim unit (PGetGlobal $ Ident "add") [])
                        [ ELConst unit (CstInt 1) ]
                    )
                  ]
                  ( ELIf unit
                      ( ELPrim unit P_log_not
                          [ ELPrim unit P_i32_le
                              [ ELVar unit (Var 2)
                              , ELConst unit (CstInt 0)
                              ]
                          ]
                      )
                      (ELApp unit (ELVar unit (Var 0)) [ ELVar unit (Var 2) ])
                      (ELApp unit (ELVar unit (Var 1)) [ ELVar unit (Var 2) ])
                  )
              )
          }
      , IR.NonRec $ Decl
          { ident: Ident "it"
          , term:
              ELApp unit
                (ELPrim unit (PGetGlobal $ Ident "f") [])
                [ ELConst unit (CstInt 42)
                ]
          }
      ]
  -- [ IR.NonRec $ Decl
  --     { "ident": Ident "it"
  --     , "term":
  --         ELLet unit
  --           [ ELConst unit (CstInt 1)
  --           , ELConst unit (CstInt 2)
  --           ]
  --           ( ELPrim unit P_i32_add
  --               [ ELVar unit (Var 1)
  --               , ELVar unit (Var 0)
  --               ]
  --           )
  --     }
  -- -- ]
  -- [ IR.NonRec $ Decl
  --     { "ident": Ident "it"
  --     , "term":
  --         ELAbs unit 1
  --           ( ELIf unit
  --               (ELVar unit (Var 0))
  --               (ELConst unit $ CstInt 1)
  --               (ELConst unit $ CstInt 2)
  --           )
  --     }
  -- ]

  -- [ IR.NonRec $ Decl
  --     { "ident": Ident "it"
  --     , "term":
  --         ELAbs unit 1
  --           ( ELPrim unit
  --               P_i32_add
  --               [ ( ELIf unit
  --                     (ELVar unit (Var 0))
  --                     (ELConst unit $ CstInt 1)
  --                     (ELConst unit $ CstInt 2)
  --                 )
  --               , ELConst unit (CstInt 42)
  --               ]
  --           )
  --     }
  -- ]
  }

main :: Effect Unit
main = launchAff_ do
  let program = lower sample
  Console.logShow program
  let gdoFile = CodeGen.mkGdoFile program
  Console.logShow gdoFile
  o <- liftEffect $ emit gdoFile
  writeFile "sample.gdo" =<< liftEffect (Buffer.fromArrayBuffer o)