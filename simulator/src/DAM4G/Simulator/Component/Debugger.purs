module DAM4G.Simulator.Component.Debugger where

import Prelude

import DAM4G.Simulator.Byte (print2Byte, printByte)
import DAM4G.Simulator.Component.Asset (assetUrls, toString)
import DAM4G.Simulator.Hooks.UseStore (useApp)
import DAM4G.Simulator.Instruction (Constant(..), Instruction(..))
import DAM4G.Simulator.Runtime as Runtime
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
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
    ctx =
      { gasm: appApi.gasm
      , program: appApi.program
      , handleRunButton
      , runtime: appApi.runtime
      , pgc: case appApi.program.code !! appApi.runtime.cpu.pgc of
          Nothing -> (-1)
          Just (n /\ _) -> n
      , breakpoints: appApi.breakpoints
      , handleBreakpointClick
      , handleStepButton
      }
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div [ HP.class_ $ ClassName "h-[100vh]" ]
      [ HH.div [ HP.class_ $ ClassName "flex" ]
          [ HH.button
              [ HP.class_ $ ClassName "text-white p-1 rounded-sm bg-pink-200" ]
              [ HH.img
                  [ HP.class_ $ ClassName ""
                  , HP.width 24
                  , HP.src $ toString assetUrls.icons.runAll
                  ]
              ]
          , HH.button
              [ HP.class_ $ ClassName "text-white p-1 rounded-sm bg-pink-200" ]
              [ HH.img
                  [ HP.class_ $ ClassName ""
                  , HE.onClick \_ -> ctx.handleRunButton
                  , HP.width 24
                  , HP.src $ toString assetUrls.icons.run
                  ]
              ]
          , HH.button
              [ HP.class_ $ ClassName "p-1 rounded-sm bg-pink-500"
              , HE.onClick \_ -> ctx.handleStepButton
              ]
              [ HH.img
                  [ HP.class_ $ ClassName ""
                  , HP.width 24
                  , HP.src $ toString assetUrls.icons.stepInto
                  ]
              ]

          ]
      , HH.div [ HP.class_ $ ClassName "max-h-[70vh] overflow-auto" ]
          [ HH.div
              [ HP.class_ $ ClassName "my-1 border border-gray-300 " ] $
              renderGasm ctx
          ]
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
                  else "bg-white hover:bg-gray-100 "
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
              ]
        ]
  renderBreak bp = do
    HH.span
      [ HP.class_ $ ClassName $ "mr-1 cursor-pointer min-w-[8px]"
          <> if bp == Enabled then " text-red-700" else " text-gray-400 "
      ]
      [ HH.text $
          if bp == NotSet then ""
          else "●"
      ]
  prettyPrintInstruction namespaces syms = case _ of
    KNoop -> "noop"
    KStop -> "stop"
    KLabel _ _ -> ""

    KQuote cst -> case cst of
      CstBool true -> "quote #01"
      CstBool false -> "quote #00"
      CstInt n -> "quote #" <> show n
    KGetGlobal (Right sym) -> "get_global " <> show sym
    KGetGlobal _ -> unsafeCrashWith "Unknown symbol"
    KSetGlobal (Right sym) -> "set_global " <> show sym
    KSetGlobal _ -> unsafeCrashWith "Unknown symbol"
    KField n -> "field<" <> show n <> ">"

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
    KUndefined n -> Fmt.fmt @"<unknown: {n}>" { n: show n }

