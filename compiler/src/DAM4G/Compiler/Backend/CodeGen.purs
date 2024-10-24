module DAM4G.Compiler.Backend.CodeGen where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import DAM4G.Compiler.Backend.CodeGen.ObjectFile (Bytecode(..), GdoFile(..), byteLength, magic)
import DAM4G.Compiler.Backend.CodeGen.ObjectFile as ObjectFile
import DAM4G.Compiler.Backend.CodeLabel (CodeLabel(..))
import DAM4G.Compiler.Backend.Instruction (Instruction(..))
import DAM4G.Compiler.Backend.Program (CodeSection(..), Program(..))
import DAM4G.Compiler.Value (Constant(..))
import DAM4G.Compiler.Version (compilerVersion)
import Data.Array (catMaybes, fold, foldMap)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.ST as STArray
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (ArrayBuffer, Int8Array)
import Data.Char (toCharCode)
import Data.Foldable (foldl, for_, sum)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)

emit :: GdoFile -> Effect ArrayBuffer
emit (GdoFile gdofile) = do
  let
    headerWithoutMagic = fold
      [ writeStringBytes gdofile.header.name
      , writeStringBytes gdofile.header.compilerVersion
      ]
    headerLength = 5 + Array.length headerWithoutMagic
    header = fold
      [ Byte <<< toCharCode <$> magic
      , [ Byte headerLength ]
      , headerWithoutMagic
      ]

    symbolSection = gdofile.syms
      <#> writeStringBytes
      # prependOffsetTable HalfWord
      # fold
      # Array.cons (HalfWord $ Array.length gdofile.syms)

    body = [ gdofile.text, gdofile.entry, symbolSection ]
      # prependOffsetTable HalfWord
      # fold
  -- sectionOffsetTable = gdmfile
  --   # \{ text, entry } ->
  --       [ 0, Array.length text, Array.length entry ]
  --         <#> (_ + headerLength)

  let
    flattenedBytes = foldMap bytesLE $ fold
      [ header
      , body
      ]
  TypedArray.buffer <$> (TypedArray.fromArray flattenedBytes :: _ (Int8Array))
  where
  bytesLE = ObjectFile.bytesLE <<< case _ of
    Sym sym
      | Just idx <- Array.findIndex (_ == sym) gdofile.syms -> Word idx
      | otherwise -> unsafeCrashWith "Impossible: undefined symbol"
    b -> b

  writeStringBytes :: String -> Array Bytecode
  writeStringBytes s = s
    # toCharArray
    <#> (toCharCode >>> Byte)
    # (_ <> [ Byte 0 ])

  prependOffsetTable :: (Int -> Bytecode) -> Array (Array Bytecode) -> Array (Array Bytecode)
  prependOffsetTable toByte bs =
    let
      ofsTbl = bs
        # foldl
            ( \acc l ->
                let
                  sectionLength = sum $ byteLength <$> l
                in
                  NonEmptyArray.snoc acc (NonEmptyArray.last acc + sectionLength)
            )
            (NonEmptyArray.singleton 0)
        <#> toByte
    in
      Array.cons (NonEmptyArray.init ofsTbl) bs

mkGdoFile :: Program -> GdoFile
mkGdoFile (Program p) = do
  let
    ofsTbl /\ textSection = writeTextSection
    entrySection = writeEntrySection
  -- syms = p.syms
  --   <#>
  --     ( unwrap
  --         >>> toCharArray
  --         >>> map (toCharCode >>> Byte)
  --         >>> (_ <> [ Byte 0 ])
  --     )
  GdoFile
    { header:
        { name: p.name
        , compilerVersion
        }
    , text: do
        let
          textOffsetTbl = p.syms
            <#> (\sym -> Map.lookup (unwrap sym) ofsTbl <#> HalfWord)
            # catMaybes
        textOffsetTbl <> textSection
    , entry: entrySection
    -- , symOfs: 
    , syms: map unwrap p.syms
    -- , symOfs: map HalfWord $ NonEmptyArray.init $ NonEmptyArray.cons' 0 $ Array.length <$> syms
    }
  where
  writeTextSection = ST.run do
    offsets <- STRef.new Map.empty
    ofsRef <- STRef.new 0
    bytes <- STArray.new
    for_ p.text \(CodeSection { code, lbl }) -> do
      let lblIndices = resolveLabels code
      ofs <- STRef.read ofsRef
      _ <- STRef.modify (Map.insert lbl ofs) offsets
      for_ code \instr -> do
        ofs' <- STRef.read ofsRef
        _ <- STArray.pushAll (opcode ofs' lblIndices (unwrap <$> p.syms) instr) bytes
        STRef.modify (_ + opcodeBytes instr) ofsRef
    code <- STArray.freeze bytes
    tbl <- STRef.read offsets
    pure (tbl /\ code)

  writeEntrySection = ST.run do
    let lblIndices = resolveLabels p.init
    ofsRef <- STRef.new 0
    bytes <- STArray.new
    for_ p.init \instr -> do
      ofs <- STRef.read ofsRef
      STArray.pushAll (opcode ofs lblIndices (unwrap <$> p.syms) instr) bytes
        *> STRef.modify (_ + opcodeBytes instr) ofsRef
    STArray.freeze bytes

  resolveLabels :: _ Instruction -> Map.Map CodeLabel Int
  resolveLabels code = code
    # foldl
        ( \(ofs /\ lbls) instr ->
            case instr of
              KLabel cl -> (ofs /\ Map.insert cl ofs lbls)
              _ -> (ofs + opcodeBytes instr) /\ lbls
        )
        (0 /\ Map.empty)
    # snd

opcode
  :: Int
  -> Map.Map CodeLabel Int
  -> Array String
  -> Instruction
  -> Array Bytecode
opcode ofs lblIndices namespaces = case _ of
  KNoop -> [ Byte 0xFE ]
  KStop -> [ Byte 0xFF ]
  KLabel _ -> []

  KQuote cst -> case cst of
    CstBool true -> [ Byte 0xC0, Byte 0x01 ]
    CstBool false -> [ Byte 0xC0, Byte 0x00 ]
    CstInt n -> [ Byte 0xC1, Word n ]
  KGetGlobal sym -> [ Byte 0xC8, Sym (unwrap sym) ]
  KSetGlobal sym -> [ Byte 0xC9, Sym (unwrap sym) ]
  KField n -> [ Byte 0xCA, Byte n ]

  KClosure ns (CodeLabel lbl)
    | Just n <- Array.findIndex (_ == ns) namespaces -> [ Byte 0x80, NamedLabel n ofs ]
    | otherwise -> unsafeCrashWith "Impossible"
  KApply -> [ Byte 0x81 ]
  KPush -> [ Byte 0x83 ]
  KPushMark -> [ Byte 0x84 ]
  KGrab -> [ Byte 0x85 ]
  KReturn -> [ Byte 0x60 ]
  KTailApply -> [ Byte 0x66 ]

  KAccess n -> [ Byte 0xE1, Byte n ]
  KLet -> [ Byte 0xE2 ]
  KEndLet n -> [ Byte 0xE3, Byte n ]
  KDummies n -> [ Byte 0xE4, Byte n ]
  KUpdate n -> [ Byte 0xE5, Byte n ]

  K_i32_add -> [ Byte 0xA1 ]
  K_i32_sub -> [ Byte 0xA2 ]
  K_i32_mul -> [ Byte 0xA3 ]
  K_i32_div -> [ Byte 0xA4 ]
  K_i32_mod -> [ Byte 0xA5 ]
  K_i32_equ -> [ Byte 0xA6 ]
  K_i32_neq -> [ Byte 0xA7 ]
  K_i32_le -> [ Byte 0xA8 ]
  K_i32_lt -> [ Byte 0xA9 ]

  K_log_and -> [ Byte 0xAA ]
  K_log_or -> [ Byte 0xAB ]
  K_log_xor -> [ Byte 0xAC ]
  K_log_not -> [ Byte 0xAD ]

  KBranch lbl -> resolveRelative 0xB0 lbl
  KBranchIf lbl -> resolveRelative 0xB1 lbl
  KBranchIfNot lbl -> resolveRelative 0xB2 lbl
  where
  resolveRelative byte lbl = case Map.lookup lbl lblIndices of
    Just n -> [ Byte byte, HalfWord (n - ofs) ]
    _ -> unsafeCrashWith "Impossible"

opcodeBytes :: Instruction -> Int
opcodeBytes = case _ of
  KLabel _ -> 0
  KQuote cst -> case cst of
    CstBool _ -> 2
    CstInt _ -> 5
  KGetGlobal _ -> 5
  KSetGlobal _ -> 5
  KField _ -> 2
  KAccess _ -> 2
  KClosure _ _ -> 5
  KEndLet _ -> 2
  KDummies _ -> 2
  KUpdate _ -> 2

  KBranch _ -> 3
  KBranchIf _ -> 3
  KBranchIfNot _ -> 3
  _ -> 1