
module Main where

import Matching
import DIMACSParser
import Tree

main :: IO ()
main = do
    let file = "data/K4.dmx"
    graph <- fileToGraph file
    print graph
    -- print $ edmonds graph
