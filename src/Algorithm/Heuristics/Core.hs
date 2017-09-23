
module Algorithm.Heuristics.Core where

import Prelude
import Algorithm.Heuristics.MaximalMatching
import Algorithm.Heuristics.ExpandContract
import qualified Parser
import Data.Graph.Core as Graph

data Heuristic = GreedyMaximal | ExpandContract | None deriving Show

runMaximalMatching :: FilePath -> IO ()
runMaximalMatching path = do
  rep <- Parser.fileToGraph path
  let graph = Graph.initialize rep
      res = maximalMatching graph
  print $ "Maximal Matching:" ++ (show $ length res)

runExpandContract :: FilePath -> IO ()
runExpandContract path = do
  rep <- Parser.fileToGraph path
  let graph = Graph.initialize rep
      res = expandContract graph
  print $  "Expand Contract:" ++ (show $ length res)
