module Main where

import Prelude ()
import Protolude
import Edmond.Algorithm.General.Core
import Edmond.Algorithm.Heuristics.ExpandContract as ExpandContract
import Edmond.Algorithm.Heuristics.MaximalMatching as MaximalMatching
import Edmond.Data.Graph.Core as Graph
import qualified Parser

-- path = "/home/lsund/Projects/edmonds/data/graphs/butterfly.dmx"
-- path = "/home/lsund/Projects/edmonds/data/graphs/peterson.dmx"
-- path = "data/graphs/bipartite.dmx"
-- path = "data/graphs/K2.dmx"
-- path = "data/graphs/P4.dmx"
path = "data/graphs/K3.dmx"
-- path = "data/graphs/K4.dmx"
-- path = "data/graphs/pbd984.dmx"
-- path = "data/graphs/lu980.dmx"
-- path = "data/graphs/ei8246.dmx"
-- path = "data/graphs/peterson.dmx"
-- path = "/home/lsund/Projects/edmonds/data/graphs/ar9152.dmx"
-- path = "data/graphs/fixed.dmx"
-- path = "data/graphs/random-graphs/haskell/100/024.dmx"

-- main :: IO ()
-- main = do
--     graph <- Parser.fileToGraph path
--     let edges = edmondsHeuristic graph
--     print $ length edges

main :: IO ()
main = do
  rep <- Parser.fileToGraph path
  let graph = Graph.initialize rep
      -- res = MaximalMatching.maximalMatching graph
      res = ExpandContract.expandContract graph
  print res
