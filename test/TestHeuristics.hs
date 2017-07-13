module TestHeuristics where

import Protolude
import Parser
import Edmond.Algorithm.Heuristics
import qualified Edmond.Data.Graph as Graph

heuristics :: IO ()
heuristics = do
    rep <- Parser.fileToGraph "data/graphs/peterson.dmx"
    let graph = Graph.initialize rep
    let res = maximalMatching graph
    print res
