module ProcessInput where

import Prelude (map, otherwise, ($), (<<<), (==))

import Data.Array (index, init, tail)
import Data.Int (fromString)
import Data.String (drop)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Utils (unsafeJust)

input :: String
input =
  """
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
"""

newtype SpecLine = SpecLine
  { state :: State
  , cuboid :: Cuboid
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

processInput :: Array SpecLine
processInput =
  map makeSpecLine $ getLines input
  where
  getLines :: String -> Array String
  getLines text = unsafeJust $ init $ unsafeJust $ tail $ lines text

makeSpecLine :: String -> SpecLine
makeSpecLine specLineStr =
  SpecLine
    { state: getState $ ps 0
    , cuboid: getCuboid $ ps 1
    }
  where
  ps :: Int -> String
  ps i = unsafeJust $ index (split (Pattern " ") specLineStr) i

  getState :: String -> State
  getState stateStr
    | stateStr == "on" = On
    | otherwise = Off

  getCuboid :: String -> Cuboid
  getCuboid cs = makeCuboid $ split (Pattern ",") cs
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
      rangeEnds dimSelector =
        Range
          { start: unsafeJust $ index ends 0
          , end: unsafeJust $ index ends 1
          }
        where
        ends = getEnds

        getEnds :: Array Int
        getEnds =
          -- NOTE: before splitting on "::", you forgot to split on "="
          -- Might do a guard check on "x=", "y=", and "z=" to assign (or not
          -- and just presume x, y, and z are in order).
          map parseInt $ (split (Pattern "::") $ dropEqual dim)
          where
          dim :: String
          dim = unsafeJust $ index dims dimSelector

          dropEqual :: String -> String
          dropEqual str =
            drop 2 str -- drop "x=" or "y=" or "z="


          parseInt :: String -> Int
          parseInt =
            unsafeJust <<< fromString

