module Main where

import Prelude
import Data.List (List(..), catMaybes, fromFoldable, (:))
import Data.Int (fromString)
import Data.Maybe (Maybe, fromJust)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

{- | 1759 is the right answer!
-}
main :: Effect Unit
main = do
  contents <- readTextFile ASCII "src/File1.txt"
  log $ show $ day1Part2 $ getNumbers contents

getNumbers :: String -> List Int
getNumbers contents =
  catMaybes $ map fromString $ fromFoldable $ lines contents

day1Part2 :: List Int -> Int
day1Part2 xs = day1 xs

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

x :: List Int
x = fromFoldable
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

