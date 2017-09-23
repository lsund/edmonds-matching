module Algorithm.Edmonds.Core where

import Prelude ()
import Protolude
import Algorithm.Edmonds.General.Core
import Algorithm.Heuristics.Core
import Algorithm.Heuristics.ExpandContract as ExpandContract
import Algorithm.Heuristics.MaximalMatching as MaximalMatching
import Data.Graph.Core as Graph
import Data.Graph hiding (path, Edge)
import qualified Parser

findMatching :: Heuristic -> Data.Graph.Graph -> Matching
findMatching heuristic rep =
  let graph = Graph.initialize rep
      initMatching =
        case heuristic of
          GreedyMaximal  -> maximalMatching graph
          ExpandContract -> expandContract graph
          None           -> []
      graph' = loadMatching graph initMatching
  in toMatching $ findRoot graph'
  
run :: FilePath -> Heuristic -> IO ()
run path heuristic = do
    rep <- Parser.fileToGraph path
    let es = findMatching heuristic rep
    putStrLn $ "Using: " ++ (show heuristic)
    print $ length es
