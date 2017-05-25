{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Edmond.Data.Graph where

import Protolude

import Logger
import Edmond.Data.Assoc
import qualified Edmond.Data.AlternatingForest as AF

import Data.Array
import qualified Data.Graph
import qualified Data.Map as Map
import qualified Data.List as List

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
                   , logger :: Logger
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
             (Logger "")
             (-1)
             (-1)

-- Logs a message
log :: Text -> Graph -> Graph
log msg graph = graph { logger = Logger.write msg (logger graph) }

-- Logs a message and a value
logv :: Show a => Text -> a -> Graph -> Graph
logv msg val graph = graph { logger = Logger.writev msg val (logger graph) }

----------------------------------------------------------------------------
-- 'Usual' Graph properties

edges :: Graph -> [Edge]
edges = Data.Graph.edges . representation

vertices :: Graph -> [Vertex]
vertices = Data.Graph.vertices . representation

neighbours :: GraphRepresentation -> Vertex -> [Vertex]
neighbours rep v = 
    let xys = zip (indices rep) (elems rep)
        forward = rep ! v
        reverse = 
            foldr (\(x, y) acc -> if v `elem` y then x : acc else acc) [] xys
    in forward ++ reverse

matching :: Graph -> [(Int, Int)]
matching graph = fst $ List.splitAt (length xs `div` 2) xs
    where xs = zip (Map.keys m) (Map.elems m)
          m = (dict . AF.mu . forest) graph

