module Main where

import Prelude
import Data.Array (catMaybes, concat, head, replicate, singleton)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (binary, fromStringAs)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.Common (joinWith)
import Data.String.Utils (length, lines)
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

setMask :: Array String -> State
setMask as =
  let mask = maskFromStr $ unsafePartial $ fromJust $ head as
  in
    State
      { bits : map convertStringToBits as
      , screwMask : mask
      }

maskFromStr :: String -> Int
maskFromStr s =
  let
    len = length s
    mask = "1" <> (joinWith "" $ concat $ replicate (len - 1) $ singleton "0")
  in
    convertStringToBits mask

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
  { bits :: Array Int
  , screwMask :: Int
  }

derive instance Generic State _
instance Show State where
  show = genericShow

newtype Diagnostic = Diagnostic
  { gamma :: Int
  , epsilon :: Int
  }

derive instance Newtype Diagnostic _
derive instance Generic Diagnostic _
instance Show Diagnostic where
  show = genericShow

-- -- Since epsilon is the complement of gamma, we don't have to
-- -- track it; can derive it at the very end.
-- calculateBits :: Array Int -> Array Diagnostic
-- calculateBits arr =
--   foldl
--     ( \(Diagnostic d) bits ->
--         Diagnostic
--           { gamma: d.gamma
--           }
--     )
--     ( Diagnostic
--         { gamma: 0
--         }
--     )
--     arr
