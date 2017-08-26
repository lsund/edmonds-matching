{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Edmond.Data.Graph.Core where

import Prelude ()
import Protolude

import Util
import qualified Edmond.Data.AlternatingForest as AF

import Data.Array
import qualified Data.Graph
import qualified Data.List as List
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set

-- Structures for holding a graph and an associated alternating forest

----------------------------------------------------------------------------
-- Graph

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type GraphRepresentation = Data.Graph.Graph
type AlternatingForest = AF.AlternatingForest

data Graph = Graph { forward     :: !Data.Graph.Graph
                   , backward    :: !Data.Graph.Graph
                   , vertices    :: ![Vertex]
                   , numVertices :: !Int
                   , forest      :: !AlternatingForest
                   , scanned     :: !IntSet
                   , currentX    :: !Vertex
                   , currentY    :: !Vertex }
----------------------------------------------------------------------------
-- Initialize

initialize :: GraphRepresentation -> Graph
initialize rep =
    let
        vertices = Data.Graph.vertices rep
        nv = length vertices
        toBackward rep =
            let redges = map swap (Data.Graph.edges rep)
            in Data.Graph.buildG (1, nv) redges
    in Graph
        rep
        (toBackward rep)
        vertices
        nv
        AF.initialize
        Set.empty
        (-1)
        (-1)

loadMatching :: Graph -> [Edge] -> Graph
loadMatching graph matching =
    let mu' = insertListSymmetric matching ((AF.mu . forest) graph)
    in graph { forest = (forest graph) { AF.mu = mu' }}

----------------------------------------------------------------------------
-- 'Usual' Graph properties

edges :: Graph -> [Edge]
edges = Data.Graph.edges . forward

containsEdges :: [Edge] -> Graph -> Bool
containsEdges es graph =
    all (\e -> containsOne [e, swap e] (edges graph)) es

neighbours :: Graph -> Vertex -> [Vertex]
neighbours graph v = (forw ! v) `List.union` (backw ! v)
    where
        forw = forward graph
        backw = backward graph

toMatching :: Graph -> [Edge]
toMatching graph = filter (uncurry (<)) xs
    where xs = zip (Map.keys mu) (Map.elems mu)
          mu = (AF.mu . forest) graph

resetForest :: Graph -> IntMap Vertex -> Graph
resetForest graph mu' =
    let newForest' = AF.initialize
    in graph { forest = newForest' { AF.mu = mu' }
             , scanned = Set.empty }

get :: IntMap Vertex -> Int -> Vertex
get m v = fromMaybe v (Map.lookup v m)
