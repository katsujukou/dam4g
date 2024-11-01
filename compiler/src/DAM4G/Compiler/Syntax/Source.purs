module DAM4G.Compiler.Syntax.Source where

import Prelude

import DAM4G.Compiler.Printer.Class (class Pretty, print)
import Data.Array (foldl)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as SCU
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Fmt as Fmt

data SourcePos = SourcePos Int Int

derive instance eqSourcePos :: Eq SourcePos
derive instance ordSourcePos :: Ord SourcePos
derive instance genericSourcePos :: Generic SourcePos _
instance showSourcePos :: Show SourcePos where
  show = genericShow

instance prettySourcePos :: Pretty SourcePos where
  print (SourcePos ln col) = Fmt.fmt @"line {ln}, column {col}" { ln, col }

infix 6 SourcePos as **

newtype SourceLoc = SourceLoc
  { from :: SourcePos
  , to :: SourcePos
  }

derive instance genericSourceLoc :: Generic SourceLoc _
derive instance Eq SourceLoc
instance showSourceLoc :: Show SourceLoc where
  show = genericShow

instance prettySourceLoc :: Pretty SourceLoc where
  print (SourceLoc { from: p1, to: p2 }) = print p1 <> " - " <> print p2

sourceLoc :: SourcePos -> SourcePos -> SourceLoc
sourceLoc from' to' = SourceLoc { from: from', to: to' }

from :: SourceLoc -> SourcePos
from (SourceLoc { from: p }) = p

to :: SourceLoc -> SourcePos
to (SourceLoc { to: p }) = p

infix 5 sourceLoc as ~

span :: SourceLoc -> SourceLoc -> SourceLoc
span (SourceLoc { from: lc1 }) (SourceLoc { to: lc2 }) = SourceLoc { from: lc1, to: lc2 }

infix 5 span as ..

type SourcePhrase r a =
  { it :: a
  , at :: SourceLoc
  | r
  }

at :: forall a. a -> SourceLoc -> SourcePhrase () a
at a = { it: a, at: _ }

infix 7 at as @@

mapPhrase :: forall a b. (a -> b) -> SourcePhrase () a -> SourcePhrase () b
mapPhrase f ph@{ it: a } = ph { it = f a }

data SourceDelta = SourceDelta Int Int

derive instance eqSourceDelta :: Eq SourceDelta
derive instance genericSourceDelta :: Generic SourceDelta _
instance showSourceDelta :: Show SourceDelta where
  show = genericShow

charDelta :: Char -> SourceDelta
charDelta = case _ of
  '\n' -> SourceDelta 1 0
  _ -> SourceDelta 0 1

appendDelta :: SourceDelta -> SourceDelta -> SourceDelta
appendDelta (SourceDelta dl1 dc1) (SourceDelta dl2 dc2) =
  case dl1, dl2 of
    _, 0 -> SourceDelta dl1 (dc1 + dc2)
    0, _ -> SourceDelta dl2 dc2
    _, _ -> SourceDelta (dl1 + dl2) dc2

stringDelta :: String -> SourceDelta
stringDelta = SCU.toCharArray >>> foldl (\acc -> appendDelta acc <<< charDelta) (SourceDelta 0 0)

advancePos :: SourcePos -> SourceDelta -> SourcePos
advancePos (SourcePos l c) (SourceDelta dl dc) = go l c dl
  where
  go l' c' = case _ of
    0 -> SourcePos l' (c' + dc)
    n -> go (l' + 1) 1 (n - 1)

emptyPos :: SourcePos
emptyPos = SourcePos 0 0

emptyLoc :: SourceLoc
emptyLoc = emptyPos ~ emptyPos