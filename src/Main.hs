
module Main where

import Protolude
import Edmond.Algorithm.Core
import Parser

-- path = "data/P4.dmx"
-- path = "data/K4.dmx"
-- path = "data/bipartite.dmx"
-- path = "data/K3.dmx"
-- path = "data/butterfly.dmx"
-- path = "data/C5.dmx"
-- path = "data/K2-v2.dmx"
-- path = "data/bipartite-simple-v2.dmx"
path = "data/graphs/peterson.dmx"

main :: IO ()
main = do
    graph <- fileToGraph path
    edmonds graph
