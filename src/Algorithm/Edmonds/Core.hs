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
findMatching heuristic dat =
  let graph = Graph.initialize (dat, False)
      initMatching =
        case heuristic of
          GreedyMaximal  -> maximalMatching graph
          ExpandContract -> expandContract graph
          None           -> []
      graph' = loadMatching graph initMatching
  in toMatching $ findRoot graph'

--------------------------------------------------------------------------------
-- IO   

run :: FilePath -> Heuristic -> IO ()
run path heuristic = do
    dat <- Parser.dimacsToGraph path
    let es = findMatching heuristic dat
    putStr $ (show $ length es :: Text)
