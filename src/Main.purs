module Main where

import Prelude
import Data.Array (catMaybes, concat, cons, head, index, replicate, singleton, updateAt)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (binary, fromStringAs, toStringAs)
import Data.Int.Bits ((.&.), complement, xor, zshr)
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
main :: Effect Unit
main = do
  log $ show $ day3 $ readTextFile UTF8 "src/sample.txt"

day3 :: String -> Int
day3 str =
  let
    readings = lines str
    s = setState $ readings
    bits = map convertStringToBits readings
    accum = calculateSums (unwrap (State s).selector) bits
    gamma = determineGammaFromSums (unwrap (Accum accum).sums)
    epsilon = gamma $ xor (unwrap (State s).mask) gamma
  in
    gamma * epsilon

setState :: Array String -> State
setState as =
  let
    len = length $ unsafeJust $ head as
  in
    State
      { selector: convertStringToBits
          $ "1" <> (joinWith "" $ concat $ replicate (len - 1) $ singleton "0")
      , mask: convertStringToBits $ joinWith ""
          $ concat
          $ replicate len
          $ singleton "1"
      , initialSums: concat $ replicate len $ singleton 0
      }

convertStringToBits :: String -> Int
convertStringToBits line =
  unsafeJust $ fromStringAs binary line

-- Since epsilon is the bitwise exclusive OR of gamma, we don't have to
-- track it; can derive it at the very end.
calculateSums :: State -> Array Int -> Array Int
calculateSums (State s) bits =
  let
    accum = Accum
      { selector: s.selector
      , sums: s.initialSums
      , offset: 0
      }
  in
    foldl
      calculateSumsForRow
      accum
      bits

-- Build sums for one reading
calculateSumsForRow :: Accum -> Int -> Accum
calculateSumsForRow (Accum accum) reading =
  foldl
    ( \acc sum ->
        let
          bump :: Int
          bump =
            if (acc.selector .&. reading >= 0)
            then 1
            else (-1)
        in
          Accum
            { selector : zshr acc.selector 1
            , sums : (unsafeJust $
                      updateAt
                        acc.offset
                        (sum + (bump :: Int))
                        acc.sums
                     )
            , offset : acc.offset + 1
            }
    )
    accum
    accum.sums

determineGammaFromSums :: Array Int -> Int
determineGammaFromSums sums =
  b2n $
    foldl
      ( \sum bin ->
          let
            bit =
              if sum >= 0 then 1 else 0
          in
            cons bit bin
      )
      []
      sums

-- Converts a binary contained in an array as 1 or 0
-- to a radix-10 Int
b2n :: Array Int -> Int
b2n = foldl (\c b -> 2 * c + b) 0

newtype State = State
  { selector :: Int
  , mask :: Int
  , initialSums :: Array Int
  }

derive instance Generic State _
instance Show State where
  show = genericShow

newtype Accum = Accum
  { selector :: Int
  , sums :: Array Int
  , offset :: Int
  }

unsafeJust :: forall a. Maybe a -> a
unsafeJust x = unsafePartial $ fromJust x

