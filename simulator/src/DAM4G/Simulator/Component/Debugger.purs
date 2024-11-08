module DAM4G.Simulator.Component.Debugger where

import Prelude

import DAM4G.Simulator.Byte (print2Byte, printByte)
import DAM4G.Simulator.Component.Asset (assetUrls, toString)
import DAM4G.Simulator.Component.StackTable as StackTable
import DAM4G.Simulator.Hooks.UseStore (useApp)
import DAM4G.Simulator.Instruction (Constant(..), Instruction(..))
import DAM4G.Simulator.Runtime as Runtime
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), joinWith, split)
import Data.String.Utils (startsWith)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Fmt as Fmt
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

data BreakpointState = NotSet | Disabled | Enabled

derive instance Eq BreakpointState
derive instance Generic BreakpointState _
instance Show BreakpointState where
  show = genericShow

make :: forall q i o m. MonadAff m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  appApi <- useApp
  let
    handleRunButton = do
      void
        $ Hooks.fork
        $ appApi.runProgram

    handleBreakpointClick addr = do
      breakpoints <- appApi.listBreakpoints
      case Map.lookup addr breakpoints of
        Nothing -> appApi.addBreakpoint addr
        Just _ -> void $ appApi.toggleBreakpoint addr

    handleStepButton = do
      { state } <- appApi.getRuntime
      case state of
        Runtime.Pause -> do
          appApi.stepProgram
        _ -> pure unit

    handleReloadButton = do
      appApi.resetRuntime

    handleEjectButton = do
      appApi.resetRuntime

    handlePauseButton = do
      appApi.togglePause
      appApi.stepProgram

    ctx =
      { gasm: appApi.gasm
      , program: appApi.program
      , handleRunButton
      , handlePauseButton
      , runtime: appApi.runtime
      , pgc: case appApi.program.code !! appApi.runtime.cpu.pgc of
          Nothing -> (-1)
          Just (n /\ _) -> n
      , isRunning: appApi.runtime.state == Runtime.Running
      , isDebugging: appApi.runtime.state == Runtime.Pause
      , isPowerOff: appApi.runtime.state == Runtime.PowerOff
      , isHalt: appApi.runtime.state == Runtime.Halt
      , breakpoints: appApi.breakpoints
      , handleBreakpointClick
      , handleStepButton
      , handleReloadButton
      , handleEjectButton
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div [ HP.class_ $ ClassName "flex flex-col h-full " ]
      [ HH.div [ HP.class_ $ ClassName "flex justify-between" ]
          [ HH.div [ HP.class_ $ ClassName "flex h-8 my-2" ]
              [ renderDebuggerButton
                  { icon: assetUrls.icons.run
                  , label: "Run"
                  , handler: ctx.handleRunButton
                  , isDisabled: ctx.isRunning || ctx.isPowerOff || ctx.isHalt
                  }
              , renderDebuggerButton
                  { icon: assetUrls.icons.runAndPause
                  , label: "Pause"
                  , handler: ctx.handlePauseButton
                  , isDisabled: ctx.isPowerOff || ctx.isHalt
                  }
              , renderDebuggerButton
                  { icon: assetUrls.icons.stepInto
                  , label: "StepIn"
                  , handler: ctx.handleStepButton
                  , isDisabled: not ctx.isDebugging || ctx.isPowerOff
                  }
              , renderDebuggerButton
                  { icon: assetUrls.icons.reload
                  , label: "Reload"
                  , handler: ctx.handleReloadButton
                  , isDisabled: ctx.isPowerOff || ctx.isRunning
                  }
              , renderDebuggerButton
                  { icon: assetUrls.icons.eject
                  , label: "Eject"
                  , handler: ctx.handleEjectButton
                  , isDisabled: ctx.isPowerOff || ctx.isRunning || ctx.isDebugging
                  }

              ]
          , HH.div [ HP.class_ $ ClassName "flex h-8 my-2" ]
              [ case ctx.runtime.state of
                  Runtime.Halt -> do
                    HH.div [ HP.class_ $ ClassName "flex items-center gap-1 text-blue-500" ]
                      [ HH.span [ HP.class_ $ ClassName "text-sm" ] [ HH.text "●" ]
                      , HH.span [ HP.class_ $ ClassName "" ] [ HH.text "halt" ]
                      ]
                  Runtime.Running -> do
                    HH.div [ HP.class_ $ ClassName "flex items-center gap-1 text-orange-500" ]
                      [ HH.span [ HP.class_ $ ClassName "text-sm" ] [ HH.text "●" ]
                      , HH.span [ HP.class_ $ ClassName "" ] [ HH.text "running" ]
                      ]
                  Runtime.Pause -> do
                    HH.div [ HP.class_ $ ClassName "flex items-center gap-1 text-red-500" ]
                      [ HH.span [ HP.class_ $ ClassName "text-sm" ] [ HH.text "●" ]
                      , HH.span [ HP.class_ $ ClassName "text-red-500" ] [ HH.text "paused" ]
                      ]
                  Runtime.PowerOff -> do
                    HH.div [ HP.class_ $ ClassName "flex items-center gap-1 text-gray-500" ]
                      [ HH.span [ HP.class_ $ ClassName "text-sm" ] [ HH.text "●" ]
                      , HH.span [] [ HH.text "power off" ]
                      ]
                  Runtime.Loaded -> do
                    HH.div [ HP.class_ $ ClassName "flex items-center gap-1 text-green-500" ]
                      [ HH.span [ HP.class_ $ ClassName "text-sm" ] [ HH.text "●" ]
                      , HH.span [ HP.class_ $ ClassName "" ] [ HH.text "loaded" ]
                      ]
                  Runtime.Glitch -> do
                    HH.div [ HP.class_ $ ClassName "flex items-center text-red-700 gap-1" ]
                      [ HH.span [ HP.class_ $ ClassName "text-sm" ] [ HH.text "●" ]
                      , HH.span [] [ HH.text "glitch" ]
                      ]
              ]
          ]
      , HH.div [ HP.class_ $ ClassName "h-[50vh] overflow-auto border border-gray-300 bg-white" ]
          $ renderGasm ctx
      , HH.div [ HP.class_ $ ClassName "flex-grow grid grid-cols-3 gap-3" ]
          [ HH.div [ HP.class_ $ ClassName "col-span-1" ]
              [ HH.div [ HP.class_ $ ClassName "h-full py-2 flex flex-col gap-2" ]
                  [ HH.div [ HP.class_ $ ClassName "flex" ]
                      [ HH.img
                          [ HP.class_ $ ClassName "mr-2"
                          , HP.width 24
                          , HP.src $ toString assetUrls.icons.cpu
                          ]
                      , HH.span [ HP.class_ $ ClassName "font-bold text-pink-500 " ]
                          [ HH.text "CPU Status" ]
                      ]
                  , HH.div [ HP.class_ $ ClassName "flex" ]
                      [ HH.span
                          [ HP.class_ $ ClassName "mr-1 text-green-700" ]
                          [ HH.text "PGC = " ]
                      , HH.div [ HP.class_ $ ClassName "flex-grow" ]
                          [ HH.div
                              [ HP.class_ $ ClassName $
                                  "font-bold text-blue-500"
                              ]
                              [ HH.text $ "$" <> print2Byte ctx.pgc ]
                          ]

                      ]
                  , HH.div [ HP.class_ $ ClassName "flex" ]
                      [ HH.span
                          [ HP.class_ $ ClassName "mr-1 text-sky-700" ]
                          [ HH.text "ACC = " ]
                      , HH.div [ HP.class_ $ ClassName "flex-grow" ] $
                          StackTable.renderTableLines [ ctx.runtime.cpu.acc ]

                      ]
                  , HH.slot_ (Proxy :: _ "ret-stack") "ret-stack" StackTable.make { label: "Ret Stack", stack: L.Nil }
                  ]
              ]
          , HH.div [ HP.class_ $ ClassName "col-span-1 pb-2" ]
              [ HH.slot_ (Proxy :: _ "arg-stack") "arg-stack" StackTable.make { label: "Arg Stack", stack: ctx.runtime.cpu.arg }
              ]
          , HH.div [ HP.class_ $ ClassName "col-span-1 pb-2" ]
              [ HH.slot_ (Proxy :: _ "env-stack") "env-stack" StackTable.make { label: "Env Stack", stack: ctx.runtime.cpu.env }
              ]
          ]
      ]

  renderDebuggerButton inp = do
    HH.button
      [ HP.class_ $ ClassName $
          "p-1 flex items-center mr-2 rounded \
          \bg-sky-700 gap-1 border border-sky-900 "
            <> (if inp.isDisabled then "opacity-50 " else " ")
      , HE.onClick \_ -> inp.handler
      ]
      [ HH.img
          [ HP.class_ $ ClassName ""
          , HP.width 16
          , HP.src $ toString inp.icon
          ]
      , HH.span [ HP.class_ $ ClassName "text-white text-sm " ] [ HH.text inp.label ]
      ]

  renderGasm ctx@{ gasm, program: { code } } = do
    code <#> \(ofs /\ instr /\ opcode) -> do
      let
        line = prettyPrintInstruction gasm.namespaces gasm.syms instr
        parts = split (Pattern " ") line
        instr' = parts !! 0
        isFocused = ctx.pgc == ofs
        breakpointState = case Map.lookup ofs ctx.breakpoints of
          Nothing -> NotSet
          Just enabled -> if enabled then Enabled else Disabled
      HH.div
        [ HP.class_ $ ClassName $
            "grid grid-cols-7 gap-5 group text-sm font-HackGenNF "
              <>
                ( if isFocused then " bg-blue-50 hover:bg-blue-100 border rounded-sm border-blue-300"
                  else "bg-white hover:bg-gray-100"
                )
        ]
        [ HH.div
            [ HP.class_ $ ClassName $
                "pl-3 flex items-center col-span-2 text-right lg:col-span-1 pr-2 group-hover:bg-blue-200 group-hover:text-yellow-600 "
                  <> (if isFocused then " text-yellow-400 bg-blue-500 " else " text-gray-500 bg-gray-200 ")
            , HE.onClick \_ -> ctx.handleBreakpointClick ofs
            ]
            [ renderBreak breakpointState
            , HH.text $ print2Byte ofs
            ]
        , HH.div [ HP.class_ $ ClassName "hidden lg:block col-span-2" ]
            [ HH.pre []
                [ HH.text $ joinWith " " $ map printByte opcode ]
            ]
        , HH.div [ HP.class_ $ ClassName "col-span-3" ] $
            if startsWith "<" line then
              [ HH.span []
                  [ HH.text line
                  ]
              ]
            else
              [ HH.span [ HP.class_ $ ClassName "text-blue-700 mr-2 font-bold" ]
                  [ HH.text $ fromMaybe "" instr'
                  ]
              , case parts !! 1 of
                  Nothing -> HH.text ""
                  Just operand
                    | startsWith "@" operand ->
                        HH.span
                          [ HP.class_ $ ClassName "text-green-700" ]
                          [ HH.text operand ]
                    | startsWith "#" operand ->
                        HH.span
                          [ HP.class_ $ ClassName "text-red-700" ]
                          [ HH.text operand ]
                    | Just _ <- Int.fromString operand ->
                        HH.span
                          [ HP.class_ $ ClassName "text-orange-500" ]
                          [ HH.text operand ]
                    | otherwise -> HH.text operand
              , case parts !! 2 of
                  Just operand2
                    | startsWith "@" operand2 ->
                        HH.span
                          [ HP.class_ $ ClassName "text-green-700" ]
                          [ HH.text $ " " <> operand2 ]
                    | Just _ <- Int.fromString operand2 ->
                        HH.span
                          [ HP.class_ $ ClassName "text-orange-500" ]
                          [ HH.text $ " " <> operand2 ]
                    | otherwise -> HH.span [] [ HH.text operand2 ]
                  Nothing -> HH.text ""
              ]
        ]
  renderBreak bp = do
    HH.span
      [ HP.class_ $ ClassName $ "mr-1 cursor-pointer min-w-[8px]"
          <> if bp == Enabled then " text-red-700" else " text-gray-400 "
      ]
      [ HH.text $
          if bp == NotSet then " "
          else "●"
      ]

  prettyPrintInstruction _ _ = case _ of
    KNoop -> "noop"
    KStop -> "stop"
    KExit -> "exit"
    KLabel _ _ -> ""

    KQuote cst -> case cst of
      CstBool true -> "quote #01"
      CstBool false -> "quote #00"
      CstInt n -> "quote #" <> show n
    KGetGlobal (Right sym) -> "get_global " <> show sym
    KGetGlobal _ -> unsafeCrashWith "Unknown symbol"
    KSetGlobal (Right sym) -> "set_global " <> show sym
    KSetGlobal _ -> unsafeCrashWith "Unknown symbol"
    KField n -> "field " <> show n <> ""
    KMakeBlock tag n -> Fmt.fmt @"makeblock #${tag} {n}" { tag: printByte tag, n: show n }
    KClosure (Right addr) -> Fmt.fmt @"closure @{addr}" { addr: print2Byte addr }
    KClosure (Left _) -> unsafeCrashWith "Unknown label"
    KApply -> "apply"
    KPush -> "push"
    KPushMark -> "pushmark"
    KGrab -> "grab"
    KReturn -> "return"
    KTailApply -> "tailapply"

    KAccess n -> Fmt.fmt @"access {n}" { n }
    KLet -> "let"
    KEndLet n -> Fmt.fmt @"endlet {n}" { n }
    KDummies n -> Fmt.fmt @"dummiese {n}" { n }
    KUpdate n -> Fmt.fmt @"update {n}" { n }

    K_i32_add -> "i32_add"
    K_i32_sub -> "i32_sub"
    K_i32_mul -> "i32_mul"
    K_i32_div -> "i32_div"
    K_i32_mod -> "i32_mod"
    K_i32_equ -> "i32_equ"
    K_i32_neq -> "i32_neq"
    K_i32_le -> "i32_le"
    K_i32_lt -> "i32_lt"

    K_log_and -> "log_and"
    K_log_or -> "log_or"
    K_log_xor -> "log_xor"
    K_log_not -> "log_not"

    KBranch toOfs -> Fmt.fmt @"branch @{ofs}" { ofs: print2Byte toOfs }
    KBranchIf toOfs -> Fmt.fmt @"branchif @{ofs}" { ofs: print2Byte toOfs }
    KBranchIfNot toOfs -> Fmt.fmt @"branchifnot @{ofs}" { ofs: print2Byte toOfs }
    KBranchIfNotImm _ (Left _) -> unsafeCrashWith "Unknown label"
    KBranchIfNotImm cst (Right toOfs) -> case cst of
      CstInt n -> Fmt.fmt @"branchifnot_imm #{cst} @{ofs}" { cst: show n, ofs: print2Byte toOfs }
      CstBool true -> Fmt.fmt @"branchifnot_imm #01 @{ofs}" { ofs: print2Byte toOfs }
      CstBool false -> Fmt.fmt @"branchifnot_imm #00 @{ofs}" { ofs: print2Byte toOfs }
    KBranchIfNotTag _ (Left _) -> unsafeCrashWith "Unknown label"
    KBranchIfNotTag tag (Right ofs) -> Fmt.fmt @"branchifnot_tag #{tag} @{ofs}" { tag: show tag, ofs: print2Byte ofs }
    KBranchIfEqTag _ (Left _) -> unsafeCrashWith "Unknown label"
    KBranchIfEqTag tag (Right ofs) -> Fmt.fmt @"branchif_tag #{tag} @{ofs}" { tag: show tag, ofs: print2Byte ofs }
    KUndefined n -> Fmt.fmt @"<unknown: {n}>" { n: show n }

