
module Main where

import Protolude
import Edmond.Algorithm.Core
import qualified Parser

-- path = "data/graphs/queen27_27.dmx"
-- path = "data/graphs/queen16_16.dmx"
-- path = "data/graphs/pma343.dmx"
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
path = "/home/lsund/Projects/edmonds/data/graphs/ar9152.dmx"
-- path = "data/graphs/fixed.dmx"
-- path = "/home/lsund/Projects/edmonds/data/graphs/random-graphs/haskell/100/024.dmx"

main :: IO ()
main = do
    graph <- Parser.fileToGraph path
    edges <- edmonds graph
    print $ length edges
