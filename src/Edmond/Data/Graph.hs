{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Edmond.Data.Graph where

import Prelude ()
import Protolude

import Util
import qualified Edmond.Data.AlternatingForest as AF

import Data.Array
import qualified Data.Graph
import qualified Data.List as List
import qualified Data.Map as Map

-- Structures for holding a graph and an associated alternating forest

----------------------------------------------------------------------------
-- Graph

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type GraphRepresentation = Data.Graph.Graph
type AlternatingForest = AF.AlternatingForest

data Graph = Graph { forward :: Data.Graph.Graph
                   , backward :: Data.Graph.Graph
                   , forest :: AlternatingForest
                   , scanned :: Map Vertex Bool 
                   , currentX :: Vertex
                   , currentY :: Vertex }
----------------------------------------------------------------------------
-- Initialize

initialize :: GraphRepresentation -> Graph
initialize rep = 
    let nv = (length . Data.Graph.vertices) rep
        ne = (length . Data.Graph.edges) rep
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in Graph rep 
             (toBackward rep)
             (AF.initialize rep)
             sInit
             (-1)
             (-1)
    where
        toBackward rep = 
            let redges = map swap (Data.Graph.edges rep)
            in Data.Graph.buildG (1, length (Data.Graph.vertices rep)) redges

loadMatching :: Graph -> [Edge] -> Graph
loadMatching graph matching =
    let xs = map fst matching
        ys = map snd matching
        mu' = adjustMapForSymmetric (zip xs ys) ((AF.mu . forest) graph)
    in graph { forest = (forest graph) { AF.mu = mu' }}

----------------------------------------------------------------------------
-- 'Usual' Graph properties

edges :: Graph -> [Edge]
edges = Data.Graph.edges . forward

vertices :: Graph -> [Vertex]
vertices = Data.Graph.vertices . forward

containsEdges :: [Edge] -> Graph -> Bool
containsEdges es graph = 
    -- all (\e -> e `elem` edges graph || swap e `elem` edges graph) es
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

resetForest :: Graph -> Map Vertex Vertex -> Graph
resetForest graph mu' =
    let newForest' = AF.initialize (forward graph)
        newScanned = Map.fromList [(x, False) | x <- [1..nv]]
    in graph { forest = newForest' { AF.mu = mu' }
             , scanned = newScanned }
    where
        nv = length $ vertices graph

