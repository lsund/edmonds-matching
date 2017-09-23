module Main where

import Prelude ()
import Protolude
import Algorithm.Edmonds.Core
import Algorithm.Heuristics.Core

-- path = "data/graphs/bipartite.dmx"
-- path = "data/graphs/K2.dmx"
-- path = "data/graphs/P4.dmx"
-- path = "data/graphs/K3.dmx"
-- path = "data/graphs/K4.dmx"
-- path = "data/graphs/OC5.dmx"
-- path = "data/graphs/OC4.dmx"
-- path = "data/graphs/pbd984.dmx"
path = "data/graphs/lu980.dmx"
-- path = "data/graphs/ei8246.dmx"
-- path = "data/graphs/peterson.dmx"
-- path = "data/graphs/ar9152.dmx"
-- path = "data/graphs/fixed.dmx"
-- path = "data/graphs/random-graphs/haskell/dense/1000/001.dmx"

main :: IO ()
main = do
  run path None
