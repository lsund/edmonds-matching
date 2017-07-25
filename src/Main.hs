
module Main where

import Protolude
import Edmond.Algorithm.Core
import qualified Parser

-- path = "/home/lsund/Projects/edmonds-matching/data/graphs/K3.dmx"
-- path = "/home/lsund/Projects/edmonds-matching/data/graphs/K2.dmx"
-- path = "/home/lsund/Projects/edmonds-matching/data/graphs/butterfly-extended.dmx"
-- path = "data/graphs/pbd984.dmx"
-- path = "data/graphs/lu980.dmx"
-- path = "data/graphs/pma343.dmx"
-- path = "data/graphs/queen6_6.dmx"
path = "data/graphs/queen7_7.dmx"
-- path = "data/graphs/queen8_8.dmx"
-- path = "data/graphs/ei8246.dmx"
-- path = "data/graphs/peterson.dmx"
-- path = "/home/lsund/Projects/edmonds/data/graphs/ar9152.dmx"
-- path = "data/graphs/fixed.dmx"
-- path = "/home/lsund/Projects/edmonds-matching/data/graphs/random-graphs/haskell/100/024.dmx"

main :: IO ()
main = do
    rep <- Parser.fileToGraph path
    edmonds rep
