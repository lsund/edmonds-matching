
module Main where

import Protolude
import Edmond.Algorithm.Core
import DIMACSParser

-- path = "data/P4.dmx"
-- path = "data/K4.dmx"
-- path = "data/bipartite.dmx"
path = "data/K3.dmx"

main :: IO ()
main = do
    graph <- fileToGraph path
    edmonds graph
