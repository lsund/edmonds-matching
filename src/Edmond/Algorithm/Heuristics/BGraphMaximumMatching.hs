
module Edmond.Algorithm.Heuristics.BGraphMaximumMatching where

import Protolude
import qualified Data.List as List
import qualified Data.Graph

import qualified Edmond.Data.Graph.Core as Graph
import Edmond.Algorithm.Bipartite.Core as BipartiteMaximumMatching

type Graph = Graph.Graph
type Edge = Data.Graph.Edge

expandEdge :: Int -> Edge -> [Edge]
expandEdge max (v, w) = [(v, w + max), (w, v + max)]

graphToBGraph :: Graph -> Graph
graphToBGraph graph =
    let edges = concatMap (expandEdge max) es
    in Graph.initialize $ Data.Graph.buildG (1, 2 * len) edges
        where
            vs = Graph.vertices graph
            es = Graph.edges graph
            len = length vs
            max = List.last vs

runHeuristic :: Graph -> [Edge]
runHeuristic graph = BipartiteMaximumMatching.run $ graphToBGraph graph
