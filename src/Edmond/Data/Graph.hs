{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Edmond.Data.Graph where

import Protolude

import Util
import Edmond.Data.Assoc
import qualified Edmond.Data.AlternatingForest as AF

import Data.Array
import qualified Data.Graph
import qualified Data.Map as Map

-- Structures for holding a graph and an associated alternating forest

----------------------------------------------------------------------------
-- Graph

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type GraphRepresentation = Data.Graph.Graph
type AlternatingForest = AF.AlternatingForest

data Graph = Graph { representation :: Data.Graph.Graph
                   , forest :: AlternatingForest
                   , scanned :: Assoc Vertex Bool 
                   , currentX :: Vertex
                   , currentY :: Vertex }

-- Initializes the graph as of the specification
initialize :: GraphRepresentation -> Graph
initialize rep = 
    let nv = (length . Data.Graph.vertices) rep
        ne = (length . Data.Graph.edges) rep
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in Graph rep 
             (AF.initialize rep)
             (makeAssoc sInit)
             (-1)
             (-1)

----------------------------------------------------------------------------
-- 'Usual' Graph properties

edges :: Graph -> [Edge]
edges = Data.Graph.edges . representation

vertices :: Graph -> [Vertex]
vertices = Data.Graph.vertices . representation

containsEdges :: [Edge] -> Graph -> Bool
containsEdges es graph = 
    -- all (\e -> e `elem` edges graph || swap e `elem` edges graph) es
    all (\e -> containsOne [e, swap e] (edges graph)) es

neighbours :: GraphRepresentation -> Vertex -> [Vertex]
neighbours rep v = 
    let xys = zip (indices rep) (elems rep)
        forward = rep ! v
        reverse = 
            foldr (\(x, y) acc -> if v `elem` y then x : acc else acc) [] xys
    in forward ++ reverse

matching :: Graph -> [(Int, Int)]
matching graph = filter (uncurry (<)) xs
    where xs = zip (Map.keys m) (Map.elems m)
          m = (dict . AF.mu . forest) graph
