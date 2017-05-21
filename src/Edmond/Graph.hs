{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Graph where

import Protolude
import Types
import Edmond.Data.Assoc
import qualified Edmond.Data.AlternatingForest as AlternatingForest

import Data.Array
import qualified Data.Graph
import qualified Data.Map as Map

-- Structures for holding a graph and an associated alternating forest

----------------------------------------------------------------------------
-- Graph

type AlternatingForest = AlternatingForest.AlternatingForest

-- todo wrap mu, phi, ro in AlternatingForest
data Graph = Graph { representation :: Data.Graph.Graph
                   , forest :: AlternatingForest
                   , scanned :: Assoc Vertex Bool }

-- Initializes the graph as of the specification
initialize :: Data.Graph.Graph -> Graph
initialize rep = 
    let nv = (length . Data.Graph.vertices) rep
        ne = (length . Data.Graph.edges) rep
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in Graph rep 
             (AlternatingForest.initialize rep)
             (makeAssoc sInit)

----------------------------------------------------------------------------
-- 'Usual' Graph properties

edges :: Graph -> [Edge]
edges = Data.Graph.edges . representation

vertices :: Graph -> [Vertex]
vertices = Data.Graph.vertices . representation

neighbours :: Data.Graph.Graph -> Vertex -> [Vertex]
neighbours rep v = rep ! v

matching :: Graph -> [(Int, Int)]
matching graph = zip (Map.keys m) (Map.elems m)
    where m = (dict . AlternatingForest.mu . forest) graph

