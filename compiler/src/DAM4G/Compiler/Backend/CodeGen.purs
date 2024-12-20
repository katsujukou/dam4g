module DAM4G.Compiler.Backend.CodeGen where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import DAM4G.Compiler.Backend.CodeGen.ObjectFile (Bytecode(..), GdoFile(..), byteLength, magic)
import DAM4G.Compiler.Backend.CodeGen.ObjectFile as ObjectFile
import DAM4G.Compiler.Backend.CodeLabel (CodeLabel(..))
import DAM4G.Compiler.Backend.Instruction (Instruction(..))
import DAM4G.Compiler.Backend.Program (CodeSection(..), Program(..))
import DAM4G.Compiler.Types (AtomicConstant(..), BlockTag(..))
import DAM4G.Compiler.Version (compilerVersion)
import Data.Array (findIndex, fold, foldMap, length)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.ST as STArray
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (ArrayBuffer, Int8Array)
import Data.Char (toCharCode)
import Data.Foldable (foldl, for_, sum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (for)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

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

    labelsSection = gdofile.lbls # Array.fold <#> HalfWord

    body = [ gdofile.text, gdofile.entry, symbolSection, labelsSection ]
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
    ofsTbl /\ textSection /\ textLabels = writeTextSection
    entrySection /\ entryLabels = writeEntrySection
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
        -- let
        --   textOffsetTbl = p.text
        --     <#> (\(CodeSection { lbl: sym }) -> Map.lookup sym ofsTbl <#> HalfWord)
        --     # catMaybes
        -- [ HalfWord $ Array.length p.text ] <> textOffsetTbl <> textSection
        textSection
    , entry: entrySection
    -- , symOfs: 
    , syms: map unwrap p.syms
    -- , symOfs: map HalfWord $ NonEmptyArray.init $ NonEmptyArray.cons' 0 $ Array.length <$> syms
    , lbls: entryLabels <> textLabels
    }
  where
  writeTextSection = ST.run do
    identOfsTbl <- STRef.new Map.empty
    ofsRef <- STRef.new 0
    bytes <- STArray.new
    labels <- for p.text \(CodeSection { code, lbl: ident }) -> do
      identOfs <- STRef.read ofsRef
      _ <- STRef.modify (Map.insert ident identOfs) identOfsTbl
      let lblIndices = resolveLabels code
      relOfsRef <- STRef.new 0
      for_ code \instr -> do
        relOfs <- STRef.read relOfsRef
        _ <- STArray.pushAll (opcode relOfs lblIndices (unwrap <$> p.syms) instr) bytes
        STRef.modify (_ + opcodeBytes instr) relOfsRef
      relOfs <- STRef.read relOfsRef
      _ <- STRef.modify (_ + relOfs) ofsRef
      pure $ writeLabels (unwrap <$> p.syms) identOfs lblIndices
    code <- STArray.freeze bytes
    tbl <- STRef.read identOfsTbl
    pure (tbl /\ code /\ fold labels)

  writeEntrySection = ST.run do
    let lblIndices = resolveLabels p.init
    ofsRef <- STRef.new 0
    bytes <- STArray.new
    for_ p.init \instr -> do
      ofs <- STRef.read ofsRef
      STArray.pushAll (opcode ofs lblIndices (unwrap <$> p.syms) instr) bytes
        *> STRef.modify (_ + opcodeBytes instr) ofsRef
    code <- STArray.freeze bytes
    pure $ code /\ (writeLabels (unwrap <$> p.syms) 0 lblIndices)

  writeLabels :: Array String -> Int -> Map.Map CodeLabel Int -> Array (Array Int)
  writeLabels namespaces identOfs = Map.toUnfoldable
    >>> map
      ( unsafePartial \((CodeLabel ns lbl) /\ ofs) ->
          [ fromMaybe (-1) (ns >>= \name -> findIndex (_ == name) namespaces)
          , lbl
          , identOfs + ofs
          ]
      )

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
opcode ofs lblIndices namespaces =
  case _ of
    KNoop -> [ Byte 0xFE ]
    KStop -> [ Byte 0xFF ]
    KExit -> [ Byte 0xEE ]
    KLabel _ -> []

    KQuote cst -> case cst of
      ACUnit -> []
      ACBool true -> [ Byte 0xC0, Byte 0x01 ]
      ACBool false -> [ Byte 0xC0, Byte 0x00 ]
      ACInt n -> [ Byte 0xC1, Word n ]
    KGetGlobal sym -> [ Byte 0xC8, Sym (unwrap sym) ]
    KSetGlobal sym -> [ Byte 0xC9, Sym (unwrap sym) ]
    KField n -> [ Byte 0xCA, Byte n ]
    KMakeBlock tag n -> [ Byte 0xCB, tagByte tag, Byte n ]

    KClosure NoLabel -> unsafeCrashWith "Impossible: Closure referencing not-a-label"
    KClosure (CodeLabel Nothing _) -> unsafeCrashWith "Impossible: Closure referencing null-namespace label"
    KClosure (CodeLabel (Just ns) lbl)
      | Just n <- Array.findIndex (_ == ns) namespaces ->
          [ Byte 0x80, NamedLabel n lbl ]
      | otherwise ->
          unsafeCrashWith "Impossible: Closure referencing Undefined labels"
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

    KBranch lbl -> resolveRelative [ Byte 0xB0 ] lbl
    KBranchIf lbl -> resolveRelative [ Byte 0xB1 ] lbl
    KBranchIfNot lbl -> resolveRelative [ Byte 0xB2 ] lbl
    KBranchIfNotImm cst lbl -> case cst of
      ACInt n -> resolveRelative [ Byte 0xB4, Word n ] lbl
      ACBool true -> resolveRelative [ Byte 0xB3, Byte 1 ] lbl
      ACBool false -> resolveRelative [ Byte 0xB3, Byte 0 ] lbl
      _ -> unsafeCrashWith "Impossible: KBranchIfNotImm unit"
    KBranchIfNotTag tag lbl -> resolveRelative [ Byte 0xB5, Byte tag ] lbl
    KBranchIfEqTag tag lbl -> resolveRelative [ Byte 0xB6, Byte tag ] lbl
  where
  resolveRelative bytes lbl =
    case Map.lookup lbl lblIndices of
      Just n -> bytes <> [ HalfWord (n - ofs) ]
      _ -> unsafeCrashWith "Impossible!"

  tagByte :: BlockTag -> Bytecode
  tagByte = case _ of
    TTuple -> Byte 0
    TConstr tag -> Byte tag

opcodeBytes :: Instruction -> Int
opcodeBytes = case _ of
  KLabel _ -> 0
  KQuote cst -> case cst of
    ACUnit -> 0
    ACBool _ -> 2
    ACInt _ -> 5
  KGetGlobal _ -> 5
  KSetGlobal _ -> 5
  KField _ -> 2
  KMakeBlock _ _ -> 3
  KAccess _ -> 2
  KClosure _ -> 5
  KEndLet _ -> 2
  KDummies _ -> 2
  KUpdate _ -> 2

  KBranch _ -> 3
  KBranchIf _ -> 3
  KBranchIfNot _ -> 3
  KBranchIfNotImm cst _
    | ACInt _ <- cst -> 7
    | otherwise -> 4
  KBranchIfNotTag _ _ -> 4
  KBranchIfEqTag _ _ -> 4
  _ -> 1