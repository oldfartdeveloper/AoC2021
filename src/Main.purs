module Main where

import Prelude
import Data.Foldable (foldl, sum)
import Data.Generic.Rep (class Generic)
import Data.List
  ( List(..)
  , catMaybes
  , concat
  , drop
  , fromFoldable
  , length
  , null
  , reverse
  , tail
  , take
  , (:)
  )
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile)

-- import Partial.Unsafe (unsafePartial)

{- | 1759 is the right answer!
-}
main :: Effect Unit
main = do
  contents <- readTextFile ASCII "src/File1.txt"
  -- log $ show $ day1 $ getNumbers contents
  log $ show $ day1 $ day1Part2 $ {- getNumbers -} sample

getNumbers :: String -> List Int
getNumbers contents =
  catMaybes $ map fromString $ fromFoldable $ lines contents

day1Part2 :: List Int -> List Int
day1Part2 lst =
  let
    token =
      foldl buildTriplets (Token { lst: lst, sums: Nil }) sample
  in
    reverse $ drop 2 $ (\(Token t) -> t.sums) token

newtype Token = Token { lst :: List Int, sums :: List Int }

derive instance Generic Token _
instance Show Token where
  show = genericShow

buildTriplets :: Token -> Int -> Token
buildTriplets (Token t) _ =
  Token
    { sums: Cons (sum $ take 3 t.lst) t.sums
    , lst: case tail t.lst of
        Just x -> x
        Nothing -> Nil
    }

day1 :: List Int -> Int
day1 Nil = 0
day1 (x : xs) = day1' x xs 0
  where
  day1' :: Int -> List Int -> Int -> Int
  day1' _ Nil acc = acc
  day1' prev (next : Nil) acc = determineIncr acc prev next
  day1' prev (next : xs') acc =
    day1' next xs'
      ( determineIncr acc prev next
      )

determineIncr :: Int -> Int -> Int -> Int
determineIncr acc prev next =
  acc +
    if prev < next then 1
    else 0

sample :: List Int
sample = fromFoldable
  [ 199
  , 200
  , 208
  , 210
  , 200
  , 207
  , 240
  , 269
  , 260
  , 263
  ]

