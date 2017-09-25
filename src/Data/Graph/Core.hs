{-# LANGUAGE OverloadedStrings #-}

module Data.Graph.Core where

import Prelude ()
import Protolude

import Util
import qualified Data.AlternatingForest as AF

import Data.Array
import qualified Data.Graph
import qualified Data.List as List
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set

--------------------------------------------------------------------------------
-- Data and Types

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type Matching = [Edge]
type GraphRepresentation = (Data.Graph.Graph, Bool)
type AlternatingForest = AF.AlternatingForest

-- A graph intended to be used only with Edmonds Blossom Algorithm. It
-- is described using the instance with the same name from the official
-- Data.Graph package.
--
-- Fields:
--
-- isBipartite - is the graph bipartite or not? (Can speed up algorithm if true)
-- forward     - the Data.Graph represented by the forward-edges
-- backward    - the Data.Graph represented by the backward-edges
-- vertices    - the set of vertices of the graph
-- numVertices - The number of vertices of the graph
-- forest      - The alternating forest as grown by the algorithm.
-- scanned     - The current set of scanned vertices.
-- currentX    - x of the current edge (x, y) being added to the tree.
-- currentY    - y of the current edge (x, y) being added to the tree.
data Graph = Graph {
     isBipartite :: Bool 
   , forward     :: !Data.Graph.Graph
   , backward    :: !Data.Graph.Graph
   , vertices    :: ![Vertex]
   , numVertices :: !Int
   , forest      :: !AlternatingForest
   , scanned     :: !IntSet
   , currentX    :: !Vertex
   , currentY    :: !Vertex }

----------------------------------------------------------------------------
-- Create

-- Initialize a tree from a GraphRepresentation
initialize :: GraphRepresentation -> Graph
initialize (dat, bipartite) =
    let
        vs = Data.Graph.vertices dat
        nv = length vs
        bes = Data.Graph.buildG (1, nv) $ map swap (Data.Graph.edges dat)
        tree = AF.initialize
    in Graph bipartite dat bes vs nv tree Set.empty (-1) (-1)

-- Load a matching into a graph
loadMatching :: Graph -> [Edge] -> Graph
loadMatching graph matching =
    let mu' = insertListSymmetric matching ((AF.mu . forest) graph)
    in graph { forest = (forest graph) { AF.mu = mu' }}

----------------------------------------------------------------------------
-- Properties

-- The edges of a graph
edges :: Graph -> [Edge]
edges = Data.Graph.edges . forward

-- The backward edges of a graph
redges :: Graph -> [Edge]
redges = Data.Graph.edges . backward

-- True if the Graph contains all of the specified edges
containsEdges :: [Edge] -> Graph -> Bool
containsEdges es graph =
    all (\e -> containsOne [e, swap e] (edges graph)) es

-- The neighbours (ie. the vertices reachable by a single edge)
-- of a vertex in the graph
neighbours :: Graph -> Vertex -> [Vertex]
neighbours graph v = (forw ! v) `List.union` (backw ! v)
    where
        forw = forward graph
        backw = backward graph

-- Convert a graph to a matching
toMatching :: Graph -> [Edge]
toMatching graph = filter (uncurry (<)) xs
    where xs = zip (Map.keys mu) (Map.elems mu)
          mu = (AF.mu . forest) graph

-- Lookup a vertex from an Intmap
get :: IntMap Vertex -> Int -> Vertex
get m v = fromMaybe v (Map.lookup v m)

--------------------------------------------------------------------------------
-- Operations

-- Reset all state variables of a graph
resetForest :: Graph -> IntMap Vertex -> Graph
resetForest graph mu' =
    let newForest' = AF.initialize
    in graph { forest = newForest' { AF.mu = mu' }
             , scanned = Set.empty }
