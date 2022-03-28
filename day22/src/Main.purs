module Main where

import Prelude (Unit, show, ($))

import Aoc22 (aoc22)

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show $ aoc22
