module DAM4G.Compiler.Printer.Class where

import Prelude

class Pretty a where
  print :: a -> String