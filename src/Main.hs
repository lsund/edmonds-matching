
module Main where

import Protolude
import Edmond.Algorithm.Core
import DIMACSParser

path = "data/P4.dmx"
-- path = "data/K4.dmx"

main :: IO ()
main = do
    graph <- fileToGraph path
    edmonds graph
