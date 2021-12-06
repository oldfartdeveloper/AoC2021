module Main where

import Prelude
import Data.List (List(..), catMaybes, fromFoldable, (:))
import Data.Int (fromString)
import Data.Maybe (Maybe, fromJust)
-- import Data.Show (show)
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
  log $ show $ increasingCount $ getNumbers contents

getNumbers :: String -> List Int
getNumbers contents =
  catMaybes $ map fromString $ fromFoldable $ lines contents

fix :: Maybe Int -> Int
fix = unsafePartial $ fromJust

calculate :: Int -> Int -> Int -> Int
calculate acc prev next = acc + (if (prev < next) then 1 else 0)

increasingCount :: List Int -> Int
increasingCount Nil = 0
increasingCount (x : xs) = increasingCount' x xs 0
  where
  increasingCount' :: Int -> List Int -> Int -> Int
  increasingCount' _ Nil acc = acc
  increasingCount' prev (next : Nil) acc = determineIncr acc prev next
  increasingCount' prev (next : xs') acc =
    increasingCount' next xs'
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

