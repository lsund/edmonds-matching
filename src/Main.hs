
module Main where

import Protolude
import Edmond.Algorithm.Core
import qualified Parser
import qualified Generator

-- path = "data/graphs/C5.dmx"
-- path = "data/P4.dmx"
-- path = "data/graphs/K4.dmx"
-- path = "data/bipartite.dmx"
-- path = "data/K3.dmx"
-- path = "data/graphs/butterfly.dmx"
-- path = "data/K2-v2.dmx"
-- path = "data/graphs/pbd984.dmx"
-- path = "data/graphs/lu980.dmx"
-- path = "data/graphs/ei8246.dmx"
-- path = "data/graphs/peterson.dmx"
-- path = "data/graphs/ar9152.dmx"
-- path = "data/graphs/fixed.dmx"
path = "data/graphs/random-graphs/haskell/08/026.dmx"

main :: IO ()
main = do
    graph <- Parser.fileToGraph path
    edges <- edmonds graph
    print $ edges
