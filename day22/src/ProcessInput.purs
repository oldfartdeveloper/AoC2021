module ProcessInput where

import Prelude

import Data.Array ((..), index, init, length, tail)
import Data.Int (fromString)
import Data.String.Common (split)
import Data.String.Pattern
import Data.String.Utils (lines)
import Utils (unsafeJust)

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

processInput inputText =
  map makeSpecLine $ getLines inputText
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
          map parseInt $ (split (Pattern "::") $ dim)
          where
          dim :: String
          dim = unsafeJust $ index dims dimSelector

          parseInt :: String -> Int
          parseInt =
            unsafeJust <<< fromString

