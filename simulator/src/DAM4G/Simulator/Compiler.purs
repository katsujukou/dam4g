module DAM4G.Simulator.Compiler where

import Prelude

import Control.Promise (Promise, toAffE)
import DAM4G.Simulator.Instruction (CodeLabel(..), Constant(..), Instruction(..))
import Data.Array (fold, (!!))
import Data.Array as Array
import Data.ArrayBuffer.Cast (toInt8Array)
import Data.ArrayBuffer.DataView (getInt16le, getInt32le, getUint16le, getUint8, remainder)
import Data.ArrayBuffer.DataView as DataView
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (ArrayBuffer, DataView, ByteOffset)
import Data.Bifunctor (lmap)
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (for, for_)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toInt)
import Effect (Effect, whileE)
import Effect.Aff (Aff, catchError)
import Effect.Aff as Exn
import Effect.Class.Console as Console
import Effect.Exception (catchException, throw)
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

type Code =
  { ofs :: Int
  , instr :: Instruction
  , opcode :: Array Int
  , label :: Maybe String
  }

type GdoFile =
  { moduleName :: String
  , compilerVersion :: String
  , syms :: Array String
  , entry :: Array Code
  , text :: Array Code
  , namespaces :: Array Int
  , lbls :: Map CodeLabel Int
  }

emptyGdoFile :: GdoFile
emptyGdoFile =
  { moduleName: ""
  , compilerVersion: ""
  , syms: []
  , entry: []
  , text: []
  , namespaces: []
  , lbls: Map.empty
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
    let symTblOfs = ofs' + symsCnt * 2
    whileE (Ref.read symsRef <#> Array.length >>> (_ < symsCnt)) do
      ofsOfs <- Ref.read ofsOfsRef
      strOfs /\ nextOfsOfs <- readInt16 bodyView ofsOfs
      Ref.write nextOfsOfs ofsOfsRef
      str /\ _ <- readAsciiString (symTblOfs + strOfs) bodyView
      Ref.modify_ (flip Array.snoc str) symsRef
    Ref.read symsRef

  -- Read list of labels
  lbls <- readLabelSection bodyView syms sectionOfsTbl.lbls
  -- load namespaces
  namespaces /\ _ <- loadNamespaces bodyView sectionOfsTbl.text symsCnt
  text /\ _ <- loadTextSection sectionOfsTbl bodyView syms
  entry /\ _ <- loadCodeBlock 0 bodyView syms (sectionOfsTbl.entry) sectionOfsTbl.syms

  pure
    { moduleName
    , compilerVersion
    , entry
    , text
    , syms
    , namespaces
    , lbls
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

  loadTextSection sectionOfsTbl bodyView syms = do
    loadCodeBlock 0 bodyView syms sectionOfsTbl.text sectionOfsTbl.entry

  loadCodeBlock ofsBody view syms from to = do
    codeList <- Ref.new []
    ofsRef <- Ref.new from
    whileE (Ref.read ofsRef <#> (_ < to)) do
      ofs <- Ref.read ofsRef
      b /\ nextOfs <- readByte view ofs
      instr /\ opcode /\ nextOfs' <- decodeOpcode view nextOfs syms b
      Ref.modify_ (flip Array.snoc { ofs, instr, opcode, label: Nothing }) codeList
      Ref.write nextOfs' ofsRef
    (/\)
      <$> Ref.read codeList
      <*> Ref.read ofsRef

  decodeOpcode view ofs syms = case _ of
    0x80 -> do
      ns /\ ofs' <- readInt16 view ofs
      lbl /\ ofs'' <- readInt16 view ofs'
      let
        cl = case syms !! ns of
          Just name -> CodeLabel name lbl
          _ -> unsafeCrashWith "Impossible"
      pure $ (KClosure $ Left cl) /\ [ 0x80, ns `mod` 256, ns / 256, lbl `mod` 256, lbl / 256 ] /\ ofs''
    0x81 -> pure $ KApply /\ [ 0x81 ] /\ ofs
    0x83 -> pure $ KPush /\ [ 0x83 ] /\ ofs
    0x84 -> pure $ KPushMark /\ [ 0x84 ] /\ ofs
    0x85 -> pure $ KGrab /\ [ 0x85 ] /\ ofs
    0x60 -> pure $ KReturn /\ [ 0x60 ] /\ ofs
    0x66 -> pure $ KTailApply /\ [ 0x66 ] /\ ofs
    0xE1 -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KAccess n) /\ [ 0xE1, n ] /\ ofs'
    0xE2 -> pure $ KLet /\ [ 0xE2 ] /\ ofs
    0xE3 -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KEndLet n) /\ [ 0xE3 ] /\ ofs'
    0xE4 -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KDummies n) /\ [ 0xE4, n ] /\ ofs'
    0xE5 -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KUpdate n) /\ [ 0xE5, n ] /\ ofs'

    0xA1 -> pure $ K_i32_add /\ [ 0xA1 ] /\ ofs
    0xA2 -> pure $ K_i32_sub /\ [ 0xA2 ] /\ ofs
    0xA3 -> pure $ K_i32_mul /\ [ 0xA3 ] /\ ofs
    0xA4 -> pure $ K_i32_div /\ [ 0xA4 ] /\ ofs
    0xA5 -> pure $ K_i32_mod /\ [ 0xA5 ] /\ ofs
    0xA6 -> pure $ K_i32_equ /\ [ 0xA6 ] /\ ofs
    0xA7 -> pure $ K_i32_neq /\ [ 0xA7 ] /\ ofs
    0xA8 -> pure $ K_i32_le /\ [ 0xA8 ] /\ ofs
    0xA9 -> pure $ K_i32_lt /\ [ 0xA9 ] /\ ofs
    0xAA -> pure $ K_log_and /\ [ 0xAA ] /\ ofs
    0xAB -> pure $ K_log_or /\ [ 0xAB ] /\ ofs
    0xAC -> pure $ K_log_xor /\ [ 0xAC ] /\ ofs
    0xAD -> pure $ K_log_not /\ [ 0xAD ] /\ ofs

    0xB0 -> do
      n /\ ofs' <- readInt16 view ofs
      pure $ (KBranch $ ofs + n - 1) /\ [ 0xB0, n `mod` 256, n / 256 ] /\ ofs'
    0xB1 -> do
      n /\ ofs' <- readInt16 view ofs
      pure $ (KBranchIf $ ofs + n - 1) /\ [ 0xB0, n `mod` 256, n / 256 ] /\ ofs'
    0xB2 -> do
      n /\ ofs' <- readInt16 view ofs
      pure $ (KBranchIfNot $ ofs + n - 1) /\ [ 0xB0, n `mod` 256, n / 256 ] /\ ofs'

    0xC0 -> do
      n /\ ofs' <- readByte view ofs

      pure $
        if n /= 0 then (KQuote $ CstBool true) /\ [ 0xC0, n ] /\ ofs'
        else (KQuote $ CstBool false) /\ [ 0xC0, n ] /\ ofs'
    0xC1 -> do
      n /\ ofs' <- readWord view ofs
      pure $ (KQuote $ CstInt n) /\ fold [ [ 0xC1 ], fourBytes n ] /\ ofs'
    0xC8 -> do
      n /\ ofs' <- readWord view ofs
      pure $ (KGetGlobal $ Left n) /\ fold [ [ 0xC8 ], fourBytes n ] /\ ofs'
    0xC9 -> do
      n /\ ofs' <- readWord view ofs
      pure $ (KSetGlobal $ Left n) /\ fold [ [ 0xC8 ], fourBytes n ] /\ ofs'

    0xCA -> do
      n /\ ofs' <- readByte view ofs
      pure $ (KField n) /\ [ 0xCA, n ] /\ ofs'

    0xFE -> pure $ KNoop /\ [ 0xFE ] /\ ofs
    0xFF -> pure $ KStop /\ [ 0xFF ] /\ ofs
    b -> pure $ KUndefined b /\ [ b ] /\ ofs
    where
    fourBytes n =
      let
        lh = n `mod` 65536
        uh = n / 65536
      in
        [ lh `mod` 256, lh / 256, uh `mod` 256, uh / 256 ]

  readSectionOffsetTable view ofs = do
    ofsTextSection /\ ofs1 <- readInt16 view ofs
    ofsEntrySection /\ ofs2 <- readInt16 view ofs1
    ofsSymbolSection /\ ofs3 <- readInt16 view ofs2
    ofsLabelSection /\ ofs4 <- readUInt16 view ofs3

    pure $
      { text: ofsTextSection
      , entry: ofsEntrySection
      , syms: ofsSymbolSection
      , lbls: ofsLabelSection
      } /\ ofs4

  readLabelSection view syms ofs = do
    lbls <- Ref.new Map.empty
    ofsRef <- Ref.new ofs
    continue <- Ref.new true
    whileE (Ref.read continue) do
      ( do
          ofs0 <- Ref.read ofsRef
          ns /\ ofs1 <- readInt16 view ofs0
          lbl /\ ofs2 <- readUInt16 view ofs1
          lblOfs /\ ofs3 <- readInt16 view ofs2

          -- 名前なし(ns==-1)のラベルはコンパイルの段階ですべて解決済み（のはず）なので、
          -- ここで記録する必要はない
          if (ns >= 0) then do
            let
              cl = case syms !! ns of
                Just sym -> CodeLabel sym lbl
                _ -> unsafeCrashWith "Impossible"
            void $ Ref.modify (Map.insert cl lblOfs) lbls
          else do
            pure unit
          Ref.write ofs3 ofsRef
      ) `catchError`
        ( \_ -> do
            Ref.write false continue
        )
    Ref.read lbls

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

  readUInt16 view ofs = do
    getUint16le view ofs >>= case _ of
      Nothing -> throw "Unexpected end of file"
      Just b -> pure $ (toInt b) /\ (ofs + 2)

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