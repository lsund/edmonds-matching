{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Graph where

import Protolude
import Types
import Edmond.Assoc

import Data.Array
import qualified Data.Graph
import qualified Data.Map as Map

-- Structures for holding a graph and an associated alternating forest

----------------------------------------------------------------------------
-- Graph
data Graph = Graph { representation :: Data.Graph.Graph
                   , mu :: Assoc Vertex Vertex
                   , phi :: Assoc Vertex Vertex
                   , ro :: Assoc Vertex Vertex
                   , scanned :: Assoc Vertex Bool }

-- Initializes the graph as of the specification
initialize :: Data.Graph.Graph -> Int -> Int -> Graph
initialize rep nv ne = 
    let idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in Graph rep 
             (makeAssoc idMap)
             (makeAssoc idMap)
             (makeAssoc idMap)
             (makeAssoc sInit)

resetButMu :: Int -> Int -> Graph -> Graph
resetButMu nv ne (Graph rep mu _ _ _ ) =
    let Graph _ _ phi ro scanned = initialize rep nv ne
    in Graph rep mu phi ro scanned

----------------------------------------------------------------------------
-- 'Usual' Graph properties

neighbours :: Data.Graph.Graph -> Vertex -> [Vertex]
neighbours rep v = rep ! v

matching :: Graph -> [(Int, Int)]
matching graph = zip (Map.keys m) (Map.elems m)
    where m = (dict . mu) graph

