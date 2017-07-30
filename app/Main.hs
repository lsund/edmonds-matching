
module Main where

import Protolude
import Edmond.Algorithm.Core
import qualified Parser

-- path = "/home/lsund/Projects/edmonds/data/graphs/K3.dmx"
-- path = "/home/lsund/Projects/edmonds/data/graphs/butterfly.dmx"
-- path = "/home/lsund/Projects/edmonds/data/graphs/peterson.dmx"
-- path = "data/K2-v2.dmx"
-- path = "data/graphs/pbd984.dmx"
path = "data/graphs/lu980.dmx"
-- path = "data/graphs/ei8246.dmx"
-- path = "data/graphs/peterson.dmx"
-- path = "/home/lsund/Projects/edmonds/data/graphs/ar9152.dmx"
-- path = "data/graphs/fixed.dmx"
-- path = "/home/lsund/Projects/edmonds/data/graphs/random-graphs/haskell/100/024.dmx"

main :: IO ()
main = do
    graph <- Parser.fileToGraph path
    edges <- edmonds graph
    print $ length edges
