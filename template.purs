module Main where

import Prelude
import Effect (Effect)
import Effect.Console(log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  contents <- readTextFile UTF8 "src/File1.txt"
  log $ show contents
