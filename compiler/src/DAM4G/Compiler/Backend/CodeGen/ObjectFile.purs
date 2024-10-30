module DAM4G.Compiler.Backend.CodeGen.ObjectFile where

import Prelude

import DAM4G.Compiler.Backend.CodeLabel (CodeLabel(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Fmt as Fmt
import Partial.Unsafe (unsafeCrashWith)

data Bytecode
  = Byte Int
  | HalfWord Int
  | Word Int
  | NamedLabel Int Int
  | Sym String

derive instance Eq Bytecode
derive instance Generic Bytecode _
instance Show Bytecode where
  show = genericShow

byteLength :: Bytecode -> Int
byteLength = case _ of
  Byte _ -> 1
  HalfWord _ -> 2
  Word _ -> 4
  NamedLabel _ _ -> 4
  Sym _ -> 4

bytesLE :: Bytecode -> Array Int
bytesLE = case _ of
  Byte n -> [ n ]
  HalfWord n -> i16LE n
  Word n -> i32LE n
  NamedLabel ns ofs -> i16LE ns <> i16LE ofs
  _ -> unsafeCrashWith "Impossible"

  where
  i16LE n = [ n `mod` 256, n `div` 256 ]
  i32LE n =
    let
      (uh /\ lh) = (n `div` 65536) /\ (n `mod` 65536)
    in
      i16LE lh <> i16LE uh

newtype GdoFile = GdoFile
  { header ::
      { name :: String
      , compilerVersion :: String
      }
  , text :: Array Bytecode
  , entry :: Array Bytecode
  , syms :: Array String
  , lbls :: Array (Array Int)
  }

derive instance Newtype GdoFile _
instance Show GdoFile where
  show (GdoFile gdofile) = Fmt.fmt @"(GdoFile {gdofile})" { gdofile: show gdofile }

magic :: Array Char
magic = [ 'G', 'A', 'S', 'M' ]

