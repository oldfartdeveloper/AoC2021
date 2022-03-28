module ProcessInput where

import Prelude

import Data.Array (init, length, tail)
import Data.String.Utils (lines)
import Utils (unsafeJust)

newtype SpecLine = SpecLine
  { state :: State
  , cubes :: Array Cube
  }

newtype State = State
  { on :: Boolean
  , off :: Boolean
  }

newtype Cube = Cube
  { x :: Range
  , y :: Range
  , z :: Range
  }

newtype Range = Range
  { start :: Int
  , end :: Int
  }

processInput inputText =
  unsafeJust $ init $ unsafeJust $ tail $ lines inputText

