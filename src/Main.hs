
module Main where

import Protolude
import Edmond.Algorithm
import DIMACSParser

path = "data/bipartite.dmx"

main :: IO ()
main = do
    graph <- fileToGraph path
    print $ edmonds graph
