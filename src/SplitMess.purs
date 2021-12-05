module SplitMess where

import Prelude
import Data.Foldable (sum)
import Data.Show (show)
import Data.String (length)
import Data.String.Common (split)
import Data.String.Pattern (Pattern)
import Effect (Effect)
import Effect.Console(log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  contents <- readTextFile ASCII "src/File1.txt"
  log $ "The number of characters is "
    <> (show $ length contents)
