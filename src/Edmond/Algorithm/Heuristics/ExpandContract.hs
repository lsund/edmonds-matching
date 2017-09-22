
module Edmond.Algorithm.Heuristics.ExpandContract where

import Prelude ()
import Protolude hiding (cycle)
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.Graph
import qualified Data.Tree as Tree

import qualified Edmond.Data.Graph.Core as Graph
import Edmond.Algorithm.Bipartite.Core as BipartiteMaximumMatching

type Graph = Graph.Graph
type Edge = Data.Graph.Edge
type Vertex = Data.Graph.Vertex
type Matching = [Edge]
type Tree = Tree.Tree

expandEdge :: Int -> Edge -> [Edge]
expandEdge maxVertex (v, w) = [(v, w + maxVertex), (w, v + maxVertex)]

expand :: Graph -> Graph
expand graph =
    let edges = concatMap (expandEdge maxVertex) es
    in Graph.initialize $ Data.Graph.buildG (1, 2 * len) edges
        where
            vs = Graph.vertices graph
            es = Graph.edges graph
            len = Graph.numVertices graph
            maxVertex = List.last vs

biDirectional :: Matching -> Matching
biDirectional = List.nub . concatMap (\(x, y) -> [(x, y), (y, x)])

flipLower :: Matching -> Matching
flipLower = map (\(x, y) -> if x > y then (y, x) else (x, y))

contract :: Graph -> Matching
contract graph =
    let brokenMatching =
          map (\(x, y) -> (x, y - nv `div` 2)) (Graph.toMatching graph)
    in biDirectional brokenMatching
    where nv = length $ Graph.vertices graph

createCycle :: [Vertex] -> Edge -> [Edge]
createCycle path e = zip path (List.tail path) ++ [e]

createPath :: [Vertex] -> [Edge]
createPath path = zip path (List.tail path)

oddElements :: [a] -> [a]
oddElements xs = oddElements' xs []
  where
    oddElements' xs acc =
      case xs of
        y : ys -> oddElements' (drop 2 xs) (y : acc)
        [] -> reverse acc

evenElements :: [a] -> [a]
evenElements xs = evenElements' xs []
  where
    evenElements' xs acc =
      case drop 1 xs of
        y : ys -> evenElements' ys (y : acc)
        [] -> reverse acc

every :: Int -> [a] -> [a]
every n xs = reverse $ every' n xs []

every' :: Int -> [a] -> [a] -> [a]
every' n xs acc =
  case drop (pred n) xs of
    y : ys -> every' n ys (y : acc)
    [] -> acc

pairElementWithTail :: a -> [a] -> [(a, a)]
pairElementWithTail e xs = [(e, x) | x <- oddElements xs]

pairsSepByEven :: [a] -> [(a, a)]
pairsSepByEven (a : b : c : xs) =
  (pairElementWithTail a xs) ++ pairsSepByEven (b : c : xs)
pairsSepByEven _ = []

-- TODO should not only be a distance of 3
oddDistance :: [a] -> [(a, a)]
oddDistance (a : b : c : xs) = (a, c) : oddDistance (b : c : xs)
oddDistance _ = []

candidates :: ([a], [a]) -> [(a, a)]
candidates (xs, ys) = [(x, y) | x <- xs, y <- ys]

backEdgesEven :: [[Vertex]] -> [(Vertex, Vertex)]
backEdgesEven levels = concatMap candidates (pairsSepByEven levels)
backEdgesOdd :: [[Vertex]] -> [(Vertex, Vertex)]
backEdgesOdd levels = concatMap candidates (oddDistance levels)

pathsToNode :: Eq a => a -> Tree a -> [[a]]
pathsToNode x (Tree.Node y ns) = [[x] | x == y] ++ map (y:) (pathsToNode x =<< ns)

allPaths :: Eq a => Tree a -> [[a]]
allPaths tree = concatMap (\x -> pathsToNode x tree) vertices
  where vertices = List.nub $ Tree.flatten tree

evenOfMinimumLength :: Int -> [Vertex] -> Bool
evenOfMinimumLength minLen path = len >= minLen && even len
  where len = length path

oddOfMinimumLength :: Int -> [Vertex] -> Bool
oddOfMinimumLength minLen path = len >= minLen && odd len
  where len = length path

firstVertex :: Graph -> Vertex
firstVertex = fromMaybe undefined . head . Graph.vertices

findPath :: Matching -> Graph -> Maybe [Edge]
findPath matching graph =
  let
    v = firstVertex graph
    dfsTree = fromMaybe undefined $ head $
              Data.Graph.dfs
              (Data.Graph.buildG (1, Graph.numVertices graph) matching) [v]
    path = find (oddOfMinimumLength 5) $ allPaths dfsTree
  in createPath <$> path


findEvenCycle :: Matching -> Graph -> Maybe [Edge]
findEvenCycle matching graph =
    let
      forest = Data.Graph.dff
               (Data.Graph.buildG (1, Graph.numVertices graph) matching)
      levelss = map Tree.levels forest
      paths = map (\t -> find (evenOfMinimumLength 4) (allPaths t)) forest
      backEdges = map (\ls -> head $ backEdgesEven ls) levelss
      brokenMaybeCycles = zip paths backEdges
      in case find bothJust brokenMaybeCycles of
        Just (path, backEdge) -> createCycle <$> path <*> backEdge
        Nothing -> Nothing
      where
        bothJust (a, b) = isJust a && isJust b

findOddCycle :: Matching -> Graph -> Maybe [Edge]
findOddCycle matching graph =
    let
      v = firstVertex graph
      dfsTree = fromMaybe undefined $ head $
                Data.Graph.dfs
                (Data.Graph.buildG (1, Graph.numVertices graph) matching) [v]
      levels = Tree.levels dfsTree
      path = find (oddOfMinimumLength 3) $ allPaths dfsTree
      backEdge = head $ backEdgesOdd levels
    in createCycle <$> path <*> backEdge

repairMatching :: Matching -> Graph -> Matching
-- repairMatching = repairPaths
repairMatching = repairEvenCycles

repairPaths :: Matching -> Graph -> Matching
repairPaths matching graph =
    case findPath matching graph of
        Nothing -> repairEvenCycles matching graph
        Just path ->
          let rest = matching \\ biDirectional path
          in repairPaths (rest ++ every 2 path) graph

repairEvenCycles :: Matching -> Graph -> Matching
repairEvenCycles matching graph =
    case findEvenCycle matching graph of
        Nothing -> repairOddCycles matching graph
        Just cycle ->
            let rest = matching \\ biDirectional cycle
            in repairEvenCycles (rest ++ every 2 cycle) graph

repairOddCycles :: Matching -> Graph -> Matching
repairOddCycles matching graph =
    case findOddCycle matching graph of
        Nothing -> matching
        Just cycle ->
            let rest = matching \\ biDirectional cycle
            in repairOddCycles (rest ++ every 2 cycle) graph

expandContract :: Graph -> Matching
expandContract graph =
    let expandedGraph = BipartiteMaximumMatching.run $ expand graph
        brokenMatching = contract expandedGraph
        repaired = repairMatching brokenMatching graph
        flipped = flipLower repaired
    in List.nub flipped
    -- in brokenMatching
    -- in repaired
