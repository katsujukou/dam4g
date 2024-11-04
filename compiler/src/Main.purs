module Main where

import Prelude

import DAM4G.Compiler.Base (baseEnv)
import DAM4G.Compiler.Syntax.Parser (parse)
import DAM4G.Syntax.WellFormedness as WF
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Class.Console as Console

src :: String
src =
  """
def set OptionInt =
  | None 
  | Some of Int

let 
  h (x: Int) (y: OptionInt) = 1;

def alias h as & (assoc right, 5)

let it = 2 * 3 + 5 & None - 7;
"""

--let it = 1 + 2 + 3 * 4 + 5 * 6 * ( 7 + 8 );

-- let flip f = fn x y -> f y x;

-- let it = flip 1 2

main :: Effect Unit
main = do
  case parse "Sample" src of
    Left err -> Console.error " Parse error" *> Console.logShow err
    Right cstModule -> do
      Console.logShow cstModule
      let Identity res = WF.runCheck baseEnv (WF.checkModule cstModule)
      Console.logShow res