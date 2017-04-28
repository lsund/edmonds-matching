
module Main where

import Matching
import DIMACSParser

main :: IO ()
main = do
    let file = "data/K4.dmx"
    graph <- fileToGraph file
    print $ edmonds graph
