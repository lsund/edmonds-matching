
module Main where

import Protolude
import Edmond.Algorithm.Core
import DIMACSParser

path = "data/bipartite-simple.dmx"

main :: IO ()
main = do
    graph <- fileToGraph path
    print $ edmonds graph
