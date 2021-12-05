module Main where

import Prelude
import Data.List (List(..), (:), filter, foldl, fromFoldable, head, length, null)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show (show)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(Tuple), fst, snd)
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
  map convertStrToInt $ fromFoldable $ lines contents

-- Note: this will crash if there's an empty line at the end
-- of the text file
convertStrToInt :: String -> Int
convertStrToInt str =
  unsafePartial (fromJust $ fromString str)

fix :: Maybe Int -> Int
fix x = unsafePartial (fromJust x)

calculate :: Int -> Int -> Int -> Int
calculate acc prev next = acc + (if (prev < next) then 1 else 0)

{- -}
increasingCount :: List Int -> Int
increasingCount Nil = 0
increasingCount (x : xs) = increasingCount' x xs 0
  where
  increasingCount' :: Int -> List Int -> Int -> Int
  increasingCount' _ Nil acc = acc
  increasingCount' prev (next : Nil) acc = acc
  increasingCount' prev (next : xs) acc =
    increasingCount' next xs
      ( acc +
          if prev < next then 1
          else 0
      )

{- -}

x = fromFoldable
  [ 186
  , 201
  , 205
  , 234
  , 236
  , 237
  , 252
  , 254
  ,
  ]
