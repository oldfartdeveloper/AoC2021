module Main where

import Prelude
import Data.Array (catMaybes)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (binary, fromStringAs)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile)

{- | ? is the right answer!
-}
main :: Effect Unit
main = do
  contents <- readTextFile ASCII "src/sample.txt"
  log $ show $ day3 $ getBits contents

newtype Diagnostic = Diagnostic
  { gamma :: Int
  , epsilon :: Int
  }

derive instance Newtype Diagnostic _
derive instance Generic Diagnostic _
instance Show Diagnostic where
  show = genericShow

day3 :: Array Int -> Int
day3 arr =
  let
    result =
      foldl
        ( \(Diagnostic d) bit ->
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
  in
    (unwrap result).gamma * (unwrap result).epsilon

convertStringToBits :: String -> Maybe Int
convertStringToBits line =
  fromStringAs binary line

getBits :: String -> Array Int
getBits deltas =
  catMaybes $ map convertStringToBits $ lines deltas
