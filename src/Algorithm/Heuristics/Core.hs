
module Algorithm.Heuristics.Core where

import Prelude ()
import Protolude
import Algorithm.Heuristics.MaximalMatching
import Algorithm.Heuristics.ExpandContract
import qualified Parser
import Data.Graph.Core as Graph

data Heuristic = GreedyMaximal | ExpandContract | None deriving Show

runMaximalMatching :: FilePath -> IO ()
runMaximalMatching path = do
  dat <- Parser.dimacsToGraph path
  let graph = Graph.initialize (dat, False)
      res = maximalMatching graph
  putStr (show res :: Text)

runExpandContract :: FilePath -> IO ()
runExpandContract path = do
  dat <- Parser.dimacsToGraph path
  let graph = Graph.initialize (dat, False)
      res = expandContract graph
  putStr (show res :: Text)
