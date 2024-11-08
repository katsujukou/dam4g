module DAM4G.Simulator.Byte where

import Prelude

import Data.Int (hexadecimal, toStringAs)
import Data.String.CodeUnits as SCU

printByte :: Int -> String
printByte n = SCU.slice (-3) (-1) $ "00" <> toStringAs hexadecimal n <> "0"

print2Byte :: Int -> String
print2Byte (-1) = "FFFF"
print2Byte n = SCU.slice (-5) (-1) ("0000" <> toStringAs hexadecimal n <> "0")

printWord :: Int -> String
printWord n = SCU.slice (-9) (-1) ("00000000" <> toStringAs hexadecimal n <> "0")