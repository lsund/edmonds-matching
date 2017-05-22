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

-- Structures for holding a graph and an associated alternating forest

----------------------------------------------------------------------------
-- Graph

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type GraphRepresentation = Data.Graph.Graph
type AlternatingForest = AF.AlternatingForest

-- todo wrap mu, phi, ro in AlternatingForest
data Graph = Graph { representation :: Data.Graph.Graph
                   , forest :: AlternatingForest
                   , scanned :: Assoc Vertex Bool 
                   , logger :: Logger }

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

log :: Graph -> Text -> Graph
log graph msg = graph { logger = Logger.write (logger graph) msg }

----------------------------------------------------------------------------
-- 'Usual' Graph properties

edges :: Graph -> [Edge]
edges = Data.Graph.edges . representation

vertices :: Graph -> [Vertex]
vertices = Data.Graph.vertices . representation

neighbours :: GraphRepresentation -> Vertex -> [Vertex]
neighbours rep v = rep ! v

matching :: Graph -> [(Int, Int)]
matching graph = zip (Map.keys m) (Map.elems m)
    where m = (dict . AF.mu . forest) graph
