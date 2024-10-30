module Main where

import Prelude

import DAM4G.Compiler.Syntax.Parser (parse)
import Effect (Effect)
import Effect.Class.Console as Console

src :: String
src =
  """let add (x: int) (y: int) = x + y;

let it = 
  let inc = add 1 
  in  inc 42
;
"""

main :: Effect Unit
main = do
  Console.logShow $ parse "Sample" src
