
module Edmond.Algorithm.Heuristics.ExpandContract where

import Prelude ()
import Protolude hiding (cycle)
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.Graph
import qualified Data.Tree as Tree
import Data.Maybe

import qualified Edmond.Data.Graph.Core as Graph
import Edmond.Algorithm.Bipartite.Core as BipartiteMaximumMatching

type Graph = Graph.Graph
type Edge = Data.Graph.Edge
type Vertex = Data.Graph.Vertex
type Matching = [Edge]

expandEdge :: Int -> Edge -> [Edge]
expandEdge max (v, w) = [(v, w + max), (w, v + max)]

expand :: Graph -> Graph
expand graph =
    let edges = concatMap (expandEdge maxVertex) es
    in Graph.initialize $ Data.Graph.buildG (1, 2 * len) edges
        where
            vs = Graph.vertices graph
            es = Graph.edges graph
            len = Graph.numVertices graph
            maxVertex = List.last vs

contract :: Graph -> Matching
contract graph =
    let brokenMatching =
          map (\(x, y) -> (x, y - nv `div` 2)) (Graph.toMatching graph)
    in filter (uncurry (<)) brokenMatching
    where nv = length $ Graph.vertices graph

dist :: Vertex -> Vertex -> [Vertex] -> Int
dist a b xs = length $ takeWhile (/= b) $ dropWhile (/= a) xs

candidates :: [Vertex] -> [Edge]
candidates path = [(y, x) | (x : ys) <- tails path, y <- ys, dist x y path >= 2]

findBackEdge :: [Vertex] -> Graph -> Maybe Edge
findBackEdge path graph =
    find (\(x, y) -> (x, y) `elem` Graph.redges graph) $ candidates path

createCycle :: [Vertex] -> Edge -> [Edge]
createCycle path e = zip path (List.tail path) ++ [e]

createPath :: [Vertex] -> [Edge]
createPath path = zip path (List.tail path)

every :: Int -> [a] -> [a]
every n xs = every' n xs []

every' :: Int -> [a] -> [a] -> [a]
every' n xs acc =
  case drop (pred n) xs of
    y : ys -> every' n ys (y : acc)
    [] -> acc

findPath :: Matching -> Graph -> Maybe [Edge]
findPath matching graph =
    let allPaths = map Tree.flatten $
          Data.Graph.dfs
          (Data.Graph.buildG (1, Graph.numVertices graph) matching) [1]
    in
        case
          find
             (\path -> odd (length path)
               && isNothing (findBackEdge path graph)) allPaths
        of
            Nothing -> Nothing
            Just path -> Just $ createPath path

findEvenCycle :: Matching -> Graph -> Maybe [Edge]
findEvenCycle matching graph =
    let allPaths =
          map Tree.flatten $
          Data.Graph.dfs
          (Data.Graph.buildG (1, Graph.numVertices graph) matching) [1]
    in
        case
          find
          (\path -> odd (length path)
            && isJust (findBackEdge path graph))
          allPaths
        of
            Nothing -> Nothing
            Just path ->
                let backEdge = fromJust $ findBackEdge path graph
                in Just $ createCycle path backEdge

findOddCycle :: Matching -> Graph -> Maybe [Edge]
findOddCycle matching graph =
    let allPaths =
          map Tree.flatten $
          Data.Graph.dfs
          (Data.Graph.buildG (1, Graph.numVertices graph) matching) [1]
    in
        case
          find
          (\path -> even (length path)
            && isJust (findBackEdge path graph)) allPaths
        of
            Nothing -> Nothing
            Just path ->
                let backEdge = fromJust $ findBackEdge path graph
                in Just $ createCycle path backEdge

repairMatching :: Matching -> Graph -> Matching
repairMatching = repairPaths

repairPaths :: Matching -> Graph -> Matching
repairPaths matching graph =
    case findPath matching graph of
        Nothing -> repairEvenCycles matching graph
        Just path ->
            let rest = matching \\ path
            in repairPaths (rest ++ every 2 path) graph

repairEvenCycles :: Matching -> Graph -> Matching
repairEvenCycles matching graph =
    case findEvenCycle matching graph of
        Nothing -> repairOddCycles matching graph
        Just cycle ->
            let rest = matching \\ cycle
            in repairEvenCycles (rest ++ every 2 cycle) graph

repairOddCycles :: Matching -> Graph -> Matching
repairOddCycles matching graph =
    case findOddCycle matching graph of
        Nothing -> matching
        Just cycle ->
            let rest = matching \\ cycle
            in repairOddCycles (rest ++ every 2 cycle) graph

expandContract :: Graph -> Matching
expandContract graph =
    let expandedGraph = BipartiteMaximumMatching.run $ expand graph
        brokenMatching = contract expandedGraph
    in repairMatching brokenMatching graph
