
module Main where

import Protolude
import Edmond.Algorithm.Core
import qualified Parser
import Second.Graph.HopcroftKarp

-- path = "data/P4.dmx"
-- path = "data/K4.dmx"
-- path = "data/bipartite.dmx"
-- path = "data/K3.dmx"
-- path = "data/graphs/butterfly.dmx"
-- path = "data/C5.dmx"
-- path = "data/K2-v2.dmx"
path = "data/graphs/pbd984.dmx"
-- path = "data/graphs/ei8246.dmx"
-- path = "data/graphs/peterson.dmx"

main :: IO ()
main = do
    graph <- Parser.fileToGraph path
    -- findMatching (edges graph)
    edmonds graph
