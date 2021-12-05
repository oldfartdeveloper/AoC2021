module Main where

import Prelude
import Data.Array (filter, length, null)
import Data.Foldable (foldl, sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show (show)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console(log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  contents <- readTextFile ASCII "src/File1.txt"
  log $ show $ length $ getNumbers contents

getNumbers :: String -> Array Int
getNumbers contents =
  map convertStrToInt $ lines contents

convertStrToInt :: String -> Int
convertStrToInt str =
  unsafePartial (fromJust $ fromString str)

-- unsafePartial (fromJust $ fromString "12")
