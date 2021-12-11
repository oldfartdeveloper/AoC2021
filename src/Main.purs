module Main where

import Prelude
import Data.Array (catMaybes, head, last)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (binary, fromStringAs, radix)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

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

day3 :: Array Int -> Int
day3 arr =
  let
    result =
      foldl
        ( \(Diagnostic d) x ->
          Diagnostic
            { gamma : 0
            , epsilon : 0
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
