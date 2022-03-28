module Aoc22 where

import Prelude

import Data.Array (init, length, tail)

import ProcessInput (processInput)


input :: String
input =
  """
on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
"""

aoc22 :: Int
aoc22 = length $ processInput input
