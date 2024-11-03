module DAM4G.Compiler
  ( CompileInput(..)
  , CompileOutput
  , compile
  , compileJs
  ) where

import Prelude

import DAM4G.Compiler.Backend.CodeGen (emit, mkGdoFile)
import DAM4G.Compiler.Backend.Lower (lower)
import DAM4G.Compiler.Name (Ident(..), ModuleName(..))
import DAM4G.Compiler.Optimizer.IR (ELC(..), Var(..))
import DAM4G.Compiler.Optimizer.IR as IR
import DAM4G.Compiler.Primitive (Primitive(..))
import DAM4G.Compiler.Value (Constant(..))
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toNullable)
import Effect (Effect)

data CompileInput
  = InpString String
  | InpFile String

type CompileOutput =
  { dasmText :: Effect String
  , emitBin :: Effect ArrayBuffer
  }

type CompileResult = Either String CompileOutput

compile :: CompileInput -> Effect CompileResult
compile _ = do
  let
    program = IR.Module
      { name: ModuleName "Sample"
      , decls:
          -- [ IR.NonRec $ IR.Decl
          --     { ident: Ident "add"
          --     , term: ELAbs unit 2
          --         ( ELPrim unit P_i32_add
          --             [ ELVar unit (Var 1)
          --             , ELVar unit (Var 0)
          --             ]
          --         )
          --     }
          -- , IR.NonRec $ IR.Decl
          --     { ident: Ident "f"
          --     , term: ELAbs unit 1
          --         ( ELLet unit
          --             [ ELAbs unit 1
          --                 ( ELPrim unit P_i32_sub
          --                     [ ELVar unit (Var 0)
          --                     , ELConst unit (CstInt 1)
          --                     ]
          --                 )
          --             , ( ELApp unit
          --                   (ELPrim unit (PGetGlobal $ Ident "add") [])
          --                   [ ELConst unit (CstInt 1) ]
          --               )
          --             ]
          --             ( ELIf unit
          --                 ( ELPrim unit P_log_not
          --                     [ ELPrim unit P_i32_le
          --                         [ ELVar unit (Var 2)
          --                         , ELConst unit (CstInt 0)
          --                         ]
          --                     ]
          --                 )
          --                 (ELApp unit (ELVar unit (Var 0)) [ ELVar unit (Var 2) ])
          --                 (ELApp unit (ELVar unit (Var 1)) [ ELVar unit (Var 2) ])
          --             )
          --         )
          --     }
          -- , IR.NonRec $ IR.Decl
          --     { ident: Ident "it"
          --     , term:
          --         ELApp unit
          --           (ELPrim unit (PGetGlobal $ Ident "f") [])
          --           [ ELConst unit (CstInt 42)
          --           ]
          --     }
          -- ]
          [ IR.NonRec $ IR.Decl
              { ident: Ident "it"
              , term:
                  ( ELLet unit
                      [ ELAbs unit 3
                          ( ELPrim unit P_i32_add
                              [ ELVar unit (Var 2)
                              , ELPrim unit P_i32_add
                                  [ ELVar unit (Var 1), ELVar unit (Var 0) ]
                              ]
                          )
                      ]
                      ( ELApp unit
                          (ELVar unit (Var 0))
                          [ ELConst unit (CstInt 1)
                          , ELConst unit (CstInt 3)
                          , ELConst unit (CstInt 5)
                          ]
                      )
                  )
              }
          ]
      -- [ IR.NonRec $ IR.Decl
      --     { "ident": Ident "that"
      --     , "term":
      --         --   ELAbs unit 1
      --         ( ELIf unit
      --             (ELConst unit $ CstBool true)
      --             (ELConst unit $ CstInt 1)
      --             (ELConst unit $ CstInt 2)
      --         )
      --     }
      -- --   ]

      -- , IR.NonRec $ IR.Decl
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
  pure $
    Right
      { dasmText: pure "hoge"
      , emitBin: program # lower >>> mkGdoFile >>> emit
      }

-- For use from JavaScript.
compileJs
  :: String
  -> Effect
       { success :: Boolean
       , error :: Nullable String
       , output :: Effect ArrayBuffer
       }
compileJs = InpString >>> compile >=> case _ of
  Left err -> pure { success: false, error: toNullable (Just err), output: AB.empty 0 }
  Right { emitBin } -> pure { success: true, error: null, output: emitBin }
