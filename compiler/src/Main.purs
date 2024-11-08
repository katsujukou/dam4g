module Main where

import Prelude

import DAM4G.Compiler.Backend.CodeGen as Codegen
import DAM4G.Compiler.Backend.Lower as Backend
import DAM4G.Compiler.Base (baseEnv)
import DAM4G.Compiler.Optimizer.Translate as TR
import DAM4G.Compiler.Syntax.Parser (parse)
import DAM4G.Syntax.WellFormedness as WF
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Effect.Exception (catchException)
import Effect.Exception as Exn

src :: String
src =
  """def set Suit = | Heart | Diamond | Club | Spade

def set Card =
  | Joker
  | Card of Suit * Int

def set Option = | None | Some of Card

let nextCard (c: Card) = match c with 
  | Joker -> None
  | Card (s, 2) -> Some Joker
  | Card (s, 13) -> Some (Card (s, 1))
  | Card (s, n) -> Some (Card(s, n + 1))
  ;

let it =
  let 
    seven_of_heart = Card(Heart, 7)
  in  
    nextCard(seven_of_heart);
"""

--let it = 1 + 2 + 3 * 4 + 5 * 6 * ( 7 + 8 );

-- let flip f = fn x y -> f y x;

-- let it = flip 1 2

main :: Effect Unit
main = do
  Console.log "======= Stage: Parsing ======="
  case parse "Sample" src of
    Left err -> Console.error " Parse error" *> Console.logShow err
    Right cstModule -> do
      Console.logShow cstModule
      Console.log "======= Stage: Well-formedness Checking ======="
      let Identity res = WF.runCheck baseEnv (WF.checkModule cstModule)
      case res of
        Left err -> logShow err
        Right (astModule /\ genv) -> do
          logShow astModule
          logShow genv
          Console.log "======= Stage: Translating ======="
          irModule /\ _ <- TR.translate genv astModule
          logShow irModule
          Console.log "======= Stage: Lowering ======="
          let program = Backend.lower irModule
          Console.logShow program
          let objfile = Codegen.mkGdoFile program
          Console.logShow objfile