module Main where

import Prelude
import Data.Array (catMaybes, replicate, singleton)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (binary, fromStringAs)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.Common (joinWith)
import Data.String.Utils (length, lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

{- | ? is the right answer!
-}
main :: Effect Unit
main = do
  contents <- readTextFile UTF8 "src/sample.txt"
  log $ show $ day3 $ getBits $ {- setMask $ -}  lines contents

-- setMask :: Array String -> State
-- setMask as =
--   let maskStr = head as
--   in

maskFromStr :: String -> Int
maskFromStr s =
  let
    len = length s
    mask = "1" <> (joinWith "" $ replicate (len - 1) $ singleton "0")
  in
    fromStringAs binary mask

getBits :: Array String -> Array Int
getBits deltas =
  catMaybes $ map convertStringToBits $ deltas

day3 :: Array Int -> Int
day3 arr =
  let
    result = calculateBits arr
  in
    (unwrap result).gamma * (unwrap result).epsilon

convertStringToBits :: String -> Maybe Int
convertStringToBits line =
  fromStringAs binary line

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

calculateBits :: Array Int -> Array Diagnostic
calculateBits arr =
  foldl
    ( \(Diagnostic d) bits ->
        Diagnostic
          { gamma: d.gamma
          , epsilon: d.epsilon
          }
    )
    ( Diagnostic
        { gamma: 0
        , epsilon: 0
        }
    )
    arr
