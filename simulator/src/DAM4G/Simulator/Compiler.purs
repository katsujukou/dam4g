module DAM4G.Simulator.Compiler where

import Prelude

import Control.Promise (Promise, toAffE)
import DAM4G.Compiler.Version (compilerVersion)
import DAM4G.Simulator.Instruction (Constant(..), Instruction(..))
import Data.Array as Array
import Data.ArrayBuffer.Cast (toInt8Array)
import Data.ArrayBuffer.DataView (getInt16le, getInt32le, getUint8, remainder)
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (ArrayBuffer, DataView, ByteOffset)
import Data.Bifunctor (lmap)
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toInt)
import Effect (Effect, whileE)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref as Ref
import Partial.Unsafe (unsafeCrashWith)

foreign import data Compiler :: Type

foreign import loadCompilerImpl :: Effect (Promise Compiler)

loadCompiler :: Aff Compiler
loadCompiler = toAffE loadCompilerImpl

foreign import compileImpl
  :: Fn2
       Compiler
       String
       (Effect ({ success :: Boolean, output :: Effect ArrayBuffer, error :: Nullable String }))

compile :: Compiler -> String -> Effect (Either String { emitBin :: Effect ArrayBuffer })
compile c src = runFn2 compileImpl c src >>= case _ of
  { success, output, error }
    | success -> pure $ Right { emitBin: output }
    | otherwise -> case toMaybe error of
        Just err -> pure $ Left err
        _ -> unsafeCrashWith "Impossible"

type GdoFile =
  { moduleName :: String
  , compilerVersion :: String
  , syms :: Array String
  , entry :: Array Instruction
  , text :: Array Instruction
  , namespaces :: Array Int
  }

emptyGdoFile :: GdoFile
emptyGdoFile =
  { moduleName: ""
  , compilerVersion: ""
  , syms: []
  , entry: [ KStop ]
  , text: []
  , namespaces: []
  }

loadGdoFile :: ArrayBuffer -> Effect GdoFile
loadGdoFile buf = do
  view <- DataView.part buf 0 4
    >>= toInt8Array
    >>= TypedArray.toArray
    >>= \bytes -> case for bytes fromCharCode of
      Just chars
        | "GASM" <- fromCharArray chars -> DataView.remainder buf 5
      _ -> throw "Not a valid gasm file."

  moduleName /\ ofsVerStr <- readAsciiString 0 view
  compilerVersion /\ ofsSecOfsTbl <- readAsciiString ofsVerStr view
  sectionOfsTbl /\ ofsBody <- readSectionOffsetTable view ofsSecOfsTbl

  bodyView <- remainder buf $ 5 + ofsBody

  -- Read list of symbols exported
  symsCnt /\ ofs' <- readInt16 bodyView sectionOfsTbl.syms
  syms <- do
    ofsOfsRef <- Ref.new ofs'
    symsRef <- Ref.new []
    let strOfsTblOfs = ofs' + symsCnt * 2
    whileE (Ref.read symsRef <#> Array.length >>> (_ < symsCnt)) do
      ofsOfs <- Ref.read ofsOfsRef
      strOfs /\ nextOfsOfs <- readInt16 bodyView ofsOfs
      Ref.write nextOfsOfs ofsOfsRef
      str /\ _ <- readAsciiString (strOfsTblOfs + strOfs) bodyView
      Ref.modify_ (flip Array.snoc str) symsRef
    Ref.read symsRef

  -- load namespaces
  namespaces /\ textOfs <- loadNamespaces bodyView sectionOfsTbl.text symsCnt
  text /\ _ <- loadCodeBlock bodyView textOfs sectionOfsTbl.entry
  entry /\ _ <- loadCodeBlock bodyView (sectionOfsTbl.entry) sectionOfsTbl.syms
  pure
    { moduleName
    , compilerVersion
    , entry
    , text
    , syms
    , namespaces
    }
  where

  loadNamespaces view from symsCnt = do
    Console.logShow from
    textOfs <- Ref.new from
    namespacesRef <- Ref.new []
    whileE (Ref.read namespacesRef <#> Array.length >>> (_ < symsCnt)) do
      ofs <- Ref.read textOfs
      ns /\ nextOfs <- readInt16 view ofs
      Ref.modify_ (flip Array.snoc ns) namespacesRef
      Ref.write nextOfs textOfs
    (/\)
      <$> Ref.read namespacesRef
      <*> Ref.read textOfs

  loadCodeBlock view from to = do
    codeList <- Ref.new []
    ofsRef <- Ref.new from
    whileE (Ref.read ofsRef <#> (_ < to)) do
      ofs <- Ref.read ofsRef
      b /\ nextOfs <- readByte view ofs
      instr /\ nextOfs' <- decodeOpcode view nextOfs b
      Ref.modify_ (flip Array.snoc instr) codeList
      Ref.write nextOfs' ofsRef
    (/\)
      <$> Ref.read codeList
      <*> Ref.read ofsRef

  decodeOpcode view ofs = case _ of
    0x80 -> do
      ns /\ ofs' <- readInt16 view ofs
      lbl /\ ofs'' <- readInt16 view ofs'
      pure $ (KClosure ns lbl) /\ ofs''
    0x81 -> pure $ KApply /\ ofs
    0x83 -> pure $ KPush /\ ofs
    0x84 -> pure $ KPushMark /\ ofs
    0x85 -> pure $ KGrab /\ ofs
    0x60 -> pure $ KReturn /\ ofs
    0x66 -> pure $ KTailApply /\ ofs
    0xE1 -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KAccess n) /\ ofs'
    0xE2 -> pure $ KLet /\ ofs
    0xE3 -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KEndLet n) /\ ofs'
    0xE4 -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KDummies n) /\ ofs'
    0xE5 -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KUpdate n) /\ ofs'

    0xA1 -> pure $ K_i32_add /\ ofs
    0xA2 -> pure $ K_i32_sub /\ ofs
    0xA3 -> pure $ K_i32_mul /\ ofs
    0xA4 -> pure $ K_i32_div /\ ofs
    0xA5 -> pure $ K_i32_mod /\ ofs
    0xA6 -> pure $ K_i32_equ /\ ofs
    0xA7 -> pure $ K_i32_neq /\ ofs
    0xA8 -> pure $ K_i32_le /\ ofs
    0xA9 -> pure $ K_i32_lt /\ ofs
    0xAA -> pure $ K_log_and /\ ofs
    0xAB -> pure $ K_log_or /\ ofs
    0xAC -> pure $ K_log_xor /\ ofs
    0xAD -> pure $ K_log_not /\ ofs

    0xB0 -> do
      n /\ ofs' <- readInt16 view ofs
      pure $ (KBranch n) /\ ofs'
    0xB1 -> do
      n /\ ofs' <- readInt16 view ofs
      pure $ (KBranchIf n) /\ ofs'
    0xB2 -> do
      n /\ ofs' <- readInt16 view ofs
      pure $ (KBranchIfNot n) /\ ofs'

    0xC0 -> do
      n /\ ofs' <- readByte view ofs
      pure $
        if n /= 0 then (KQuote $ CstBool true) /\ ofs'
        else (KQuote $ CstBool false) /\ ofs'
    0xC1 -> do
      n /\ ofs' <- readWord view ofs
      pure $ (KQuote $ CstInt n) /\ ofs'
    0xC8 -> do
      n /\ ofs' <- readWord view ofs
      pure $ (KGetGlobal n) /\ ofs'
    0xC9 -> do
      n /\ ofs' <- readWord view ofs
      pure $ (KSetGlobal n) /\ ofs'

    0xCA -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KField n) /\ ofs'

    0xFE -> pure $ KNoop /\ ofs
    0xFF -> pure $ KStop /\ ofs
    b -> pure $ KUndefined b /\ ofs

  readSectionOffsetTable view ofs = do
    ofsTextSection /\ ofs1 <- readInt16 view ofs
    ofsEntrySection /\ ofs2 <- readInt16 view ofs1
    ofsSymbolSection /\ ofs3 <- readInt16 view ofs2
    pure $
      { text: ofsTextSection
      , entry: ofsEntrySection
      , syms: ofsSymbolSection
      } /\ ofs3

  readByte view ofs = readUInt8 view ofs <#> lmap toInt

  readWord view ofs = do
    getInt32le view ofs >>= case _ of
      Just n -> pure $ n /\ (ofs + 4)
      Nothing -> throw "Unexpected end of file"

  readUInt8 view ofs = do
    getUint8 view ofs >>= case _ of
      Nothing -> throw "unexpected end of file"
      Just b -> pure $ b /\ (ofs + 1)

  readInt16 view ofs = do
    getInt16le view ofs >>= case _ of
      Nothing -> throw "Unexpected end of file"
      Just b -> pure $ b /\ (ofs + 2)

  readAsciiString :: ByteOffset -> DataView -> Effect (String /\ ByteOffset)
  readAsciiString = go []
    where
    go chars ofs view = do
      DataView.getInt8 view ofs >>= case _ of
        Nothing -> throw "Unexpected end of file"
        Just b
          | b == 0 -> do
              pure $ fromCharArray chars /\ (1 + ofs)
          | Just ch <- fromCharCode b -> do
              go (Array.snoc chars ch) (ofs + 1) view
          | otherwise -> throw $ "Invalid char code: " <> show b