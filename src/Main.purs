module Main where

import Prelude
import Data.Array (catMaybes, concat, head, replicate, singleton)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (binary, fromStringAs, toStringAs)
import Data.Int.Bits (complement)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (length)
import Data.String.Common (joinWith)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

{- | ? is the right answer!
-}
-- main :: Effect Unit
-- main = do
--   contents <- readTextFile UTF8 "src/sample.txt"
--   log $ show $ day3 $ getBits $ {- setMask $ -}  lines contents

setState :: Array String -> State
setState as =
  let
    len = length $ unsafePartial $ fromJust $ head as
  in
    State
      { selector : convertStringToBits
          $ "1" <> (joinWith "" $ concat $ replicate (len - 1) $ singleton "0")
      , mask : convertStringToBits $ joinWith ""
          $ concat $ replicate len $ singleton "1"
      , bits : map convertStringToBits as
      , sums : []
      }

getBits :: Array String -> Array Int
getBits deltas =
  map convertStringToBits deltas

-- day3 :: Array Int -> Int
-- day3 arr =
--   let
--     result = calculateBits arr
--   in
--     (unwrap result).gamma * (unwrap result).epsilon

convertStringToBits :: String -> Int
convertStringToBits line =
  unsafePartial $ fromJust $ fromStringAs binary line

newtype State = State
  { bits :: Array Int -- the input diagnostics
  , selector :: Int
  , mask :: Int
   -- How many more gammas than epsilons for each bit column; can be negative:
  , sums :: Array Int
  }

derive instance Generic State _
instance Show State where
  show = genericShow

-- Since epsilon is the complement of gamma, we don't have to
-- track it; can derive it at the very end.
calculateBits :: State -> State
calculateBits (State s) =
  foldl
    ( \sums bits ->
        State s
    )
    (State s)
    s.bits

