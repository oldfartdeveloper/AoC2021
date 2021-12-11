module Main where

import Prelude
import Data.Array (head, last, tail)
import Data.Foldable (foldl, sum)
import Data.Generic.Rep (class Generic)
-- import Data.List
--   ( List(..)
--   , catMaybes
--   , concat
--   , drop
--   , fromFoldable
--   , length
--   , null
--   , reverse
--   , tail
--   , take
--   , (:)
--   )
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.String.Utils (lines)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)


{- | ? is the right answer!
-}
main :: Effect Unit
main = do
  contents <- readTextFile ASCII "src/sample.txt"
  log $ show $ day2 $ getDeltas contents

getDeltas :: String -> Array DirType
getDeltas deltas =
  map convertStringToToken $ lines deltas

day2 :: Array DirType -> Int
day2 arr =
  let
    result =
      foldl (\acc (DirType dt) ->
              case dt.dir of
                Forward -> Tuple
                  ((fst acc) + dt.distance)
                  (snd acc)
                Down -> Tuple
                  (fst acc)
                  ((snd acc) + dt.distance)
                Up -> Tuple
                  (fst acc)
                  ((snd acc) - dt.distance)
            ) (Tuple 0 0) arr
  in
    (fst result) * (snd result)

convertStringToToken :: String -> DirType
convertStringToToken line =
  let
    pair = split (Pattern " ") line
    dir = unsafePartial $ fromJust $ head pair
    distance = unsafePartial $ fromJust $ last pair
    dst = unsafePartial $ fromJust $ fromString distance
  in
    case dir of
      "forward" -> DirType { dir : Forward, distance : dst }
      "down"    -> DirType { dir : Down,    distance : dst }
      "up"      -> DirType { dir : Up,      distance : dst }
      _         -> DirType { dir : Forward, distance : 0   } -- do nothing

data Dir
  = Forward
  | Down
  | Up

derive instance Generic Dir _
instance Show Dir where
  show = genericShow

newtype DirType = DirType { dir :: Dir, distance :: Int }

derive instance Generic DirType _
instance Show DirType where
  show = genericShow

sample :: String
sample = """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""

foo :: Int
foo = 5
