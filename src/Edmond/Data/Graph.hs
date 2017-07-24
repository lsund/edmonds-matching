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
import qualified Data.HashMap.Strict as HashMap

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
                   , scanned :: HashMap Vertex Bool 
                   , currentX :: Vertex
                   , currentY :: Vertex }

data Property = Mu | Phi | Ro | Scanned
getVertex :: Graph -> Property -> Vertex -> Vertex
getVertex graph Mu v = AF.mu (forest graph) HashMap.! v
getVertex graph Phi v = AF.phi (forest graph) HashMap.! v
getVertex graph Ro v = AF.ro (forest graph) HashMap.! v

getScanned :: Graph -> Vertex -> Bool
getScanned graph v = scanned graph HashMap.! v

----------------------------------------------------------------------------
-- Initialize

initialize :: GraphRepresentation -> Graph
initialize rep = 
    let nv = (length . Data.Graph.vertices) rep
        ne = (length . Data.Graph.edges) rep
        sInit = HashMap.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
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
    where xs = zip (HashMap.keys mu) (HashMap.elems mu)
          mu = (AF.mu . forest) graph


resetForest :: Graph -> Graph
resetForest graph =
    let nv = length $ vertices graph 
        forest' = (AF.initialize (forward graph)) { AF.mu = AF.mu (forest graph) }
        scanned' = HashMap.fromList [(x, False) | x <- [1..nv]]
    in graph { forest = forest'
             , scanned = scanned' }


update :: Graph -> Property -> [(Vertex, Vertex)] -> Graph
update graph Ro xs = 
    graph { forest = (forest graph) { AF.ro = ro' } }
    where ro' = adjustMapFor xs $ (AF.ro . forest) graph

updateSymmetric :: Graph -> Property -> [(Vertex, Vertex)] -> Graph
updateSymmetric graph Phi xs = 
    graph { forest = (forest graph) { AF.phi = phi' } }
    where phi' = adjustMapForSymmetric xs $ (AF.phi . forest) graph
updateSymmetric graph Mu xs = 
    graph { forest = (forest graph) { AF.mu = mu' } }
    where mu' = adjustMapForSymmetric xs $ (AF.mu . forest) graph

updateVertex :: Graph -> Property -> (Vertex, Vertex) -> Graph
updateVertex graph Phi (k, v) = graph { forest = forest' }
    where forest' = (forest graph) { AF.phi = adjustMap k v phi}
          phi = (AF.phi . forest) graph

