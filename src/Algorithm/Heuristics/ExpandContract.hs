module Algorithm.Heuristics.ExpandContract where

import Prelude ()
import Protolude hiding (cycle, some)
import qualified Data.List as List
import qualified Data.Graph
import qualified Data.Tree as Tree
import Util (oddElements)

import qualified Data.Graph.Core as Graph
import Data.Graph.Core (Graph, Vertex, Edge, Matching)

import Algorithm.Edmonds.General.Core

data Component = Path | EvenCycle | OddCycle deriving Eq
type Tree = Tree.Tree
type Forest a = Tree.Forest a

expandEdge :: Int -> Edge -> [Edge]
expandEdge maxVertex (v, w) = [(v, w + maxVertex), (w, v + maxVertex)]

expand :: Graph -> Graph
expand graph =
    let edges = concatMap (expandEdge maxVertex) es
    in Graph.initialize $ (Data.Graph.buildG (1, 2 * len) edges, True)
        where
            vs = Graph.vertices graph
            es = Graph.edges graph
            len = Graph.numVertices graph
            maxVertex = List.last vs

contract :: Graph -> Matching
contract graph =
    let brokenMatching =
          map (\(x, y) -> (x, y - nv `div` 2)) (Graph.toMatching graph)
    in brokenMatching
    where nv = length $ Graph.vertices graph

verticesToPath :: [Vertex] -> [Edge]
verticesToPath []           = []
verticesToPath [_]          = []
verticesToPath (v : w : vs) = (v, w) : verticesToPath (w : vs)
        
repairTree :: Tree Vertex -> [Edge]
repairTree t = (oddElements . verticesToPath . Tree.flatten) t

repair :: [Edge] -> Graph -> Matching
repair matching graph =
  let
    forest = Data.Graph.dff
             (Data.Graph.buildG (1, Graph.numVertices graph) matching)
    res = concatMap repairTree forest
  in res

expandContract :: Graph -> Matching
expandContract graph =
    let expandedGraph = findRoot $ expand graph
        brokenMatching = contract expandedGraph
        repaired = repair brokenMatching graph
    in repaired
