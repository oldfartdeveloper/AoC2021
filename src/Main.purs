module Main where

import Prelude
import Data.Array (head, last)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (fromJust)
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
  log $ show $ day2 $ getDeltas contents

getDeltas :: String -> Array DirType
getDeltas deltas =
  map convertStringToToken $ lines deltas

day2 :: Array DirType -> Int
day2 arr =
  let
    result =
      foldl
        ( \(Displacements d) (DirType dt) ->
            case dt.dir of
              Forward -> Displacements
                { horiz: d.horiz + dt.distance
                , aim: d.aim
                , depth: d.depth + (d.aim * dt.distance)
                }
              Down -> Displacements
                { horiz: d.horiz
                , aim: d.aim + dt.distance
                , depth: d.depth
                }
              Up -> Displacements
                { horiz: d.horiz
                , aim: d.aim - dt.distance
                , depth: d.depth
                }
        )
        ( Displacements
            { horiz: 0
            , depth: 0
            , aim: 0
            }
        )
        arr
  in
    (unwrap result).horiz * (unwrap result).depth

convertStringToToken :: String -> DirType
convertStringToToken line =
  let
    pair = split (Pattern " ") line
    dir = unsafePartial $ fromJust $ head pair
    distance = unsafePartial $ fromJust $ last pair
    dst = unsafePartial $ fromJust $ fromString distance
  in
    case dir of
      "forward" -> DirType { dir: Forward, distance: dst }
      "down" -> DirType { dir: Down, distance: dst }
      "up" -> DirType { dir: Up, distance: dst }
      _ -> DirType { dir: Forward, distance: 0 } -- do nothing

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

newtype Displacements = Displacements
  { horiz :: Int, depth :: Int, aim :: Int }

derive instance Generic Displacements _
instance Show Displacements where
  show = genericShow

derive instance Newtype Displacements _

sample :: String
sample =
  """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""
