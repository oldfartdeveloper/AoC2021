module ProcessInput where

import Prelude

import Data.Array ((..), init, length, tail)
import Data.Int (fromString)
import Data.String.Common (split)
import Data.String.Pattern
import Data.String.Utils (lines)
import Utils (unsafeJust)

newtype SpecLine = SpecLine
  { state :: State
  , cuboid :: Array Cuboid
  }

data State
  = On
  | Off

newtype Cuboid = Cuboid
  { x :: Range
  , y :: Range
  , z :: Range
  }

newtype Range = Range
  { start :: Int
  , end :: Int
  }

processInput inputText =
  map makeSpecLine $ getLines inputText
  where
  getLines :: String -> Array String
  getLines = unsafeJust $ init $ unsafeJust $ tail

makeSpecLine :: String -> SpecLine
makeSpecLine s =
  SpecLine
    { state: getState ps
    , cuboid: getCuboid ps
    }
  where
  ps = split (Pattern " ") s
  getState s'
    | s' == "on" = On
    | otherwise = Off
  getCuboid cs = map makeCuboid $ split "," cs
    where
    makeCuboid :: Array String -> Cuboid
    makeCuboid dims =
      Cuboid
        { x: rangeEnds 0
        , y: rangeEnds 1
        , z: rangeEnds 2
        }
      where
      rangeEnds :: Int -> Range
      rangeEnds i =
        Range
          { start: ends [ 0 ]
          , end: ends [ 1 ]
          }
        where
        ends = getEnds i

        getEnds :: Int -> Array Int
        getEnds i' = map fromString (split (Pattern "::") dims [ i' ])

