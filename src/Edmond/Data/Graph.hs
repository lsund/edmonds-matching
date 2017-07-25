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
import qualified Data.HashTable.ST.Basic as HashTable

-- Structures for holding a graph and an associated alternating forest

----------------------------------------------------------------------------
-- Graph

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type GraphRepresentation = Data.Graph.Graph
type AlternatingForest = AF.AlternatingForest

data Graph s = Graph { forward   :: Data.Graph.Graph
                     , backward  :: Data.Graph.Graph
                     , forest    :: AlternatingForest s
                     , scanned   :: HashTable s Vertex Bool
                     , dimension :: (Int, Int)
                     , currentX  :: Vertex
                     , currentY  :: Vertex }

data Property = Mu | Phi | Ro
----------------------------------------------------------------------------
-- Initialize

initialize :: GraphRepresentation -> ST s (Graph s)
initialize rep = do
    sInit <- HashTable.new
    let nv = (length . Data.Graph.vertices) rep
        ne = (length . Data.Graph.edges) rep
    initForest <- AF.initialize rep
    return $
        Graph rep 
            (toBackward rep)
            initForest 
            sInit
            (nv, ne)
            (-1)
            (-1)
    where
        toBackward rep = 
            let redges = map swap (Data.Graph.edges rep)
            in Data.Graph.buildG (1, length (Data.Graph.vertices rep)) redges

loadMatching :: ST s (Graph s) -> [Edge] -> ST s (Graph s)
loadMatching graph matching = do
    let xs = map fst matching
        ys = map snd matching
    updateSymmetric graph Mu (zip xs ys)

----------------------------------------------------------------------------
-- 'Usual' Graph properties

edges :: ST s (Graph s) -> ST s [Edge]
edges graph = graph >>= return . Data.Graph.edges . forward

vertices :: ST s (Graph s) -> ST s [Vertex]
vertices graph = graph >>= return . Data.Graph.vertices . forward

containsEdges :: [Edge] -> ST s (Graph s) -> ST s Bool
containsEdges es graph = do
    graphEdges <- edges graph
    return $ all (\e -> containsOne [e, swap e] graphEdges) es

neighbours :: ST s (Graph s) -> Vertex -> ST s [Vertex]
neighbours graph v = do
    graph' <- graph
    let forw = forward graph'
        backw = backward graph'
    return $ (forw ! v) `List.union` (backw ! v)

-- toMatching :: ST s (Graph s) -> ST s [Edge]
-- toMatching graph = do
--     graph' <-  graph
--     let mu = (AF.mu . forest) graph'
--         xs = zip (HashMap.keys mu) (HashMap.elems mu)
--     return $ filter (uncurry (<)) xs

----------------------------------------------------------------------------
-- API for AlternatingForest

toMatching :: ST s (Graph s) -> ST s [Edge]
toMatching graph = do
    graph' <- graph
    let mu = (AF.mu . forest) graph'
    HashTable.foldM (\acc (k, v) -> return ((k, v) : acc)) [] mu

reset :: ST s (Graph s) -> ST s (Graph s)
reset graph = do
    graph' <- graph
    scanned' <- HashTable.new
    let nv = (fst . dimension) graph'
    init <- AF.initialize (forward graph')
    let forest' = init { AF.mu = AF.mu (forest graph') }
    return graph' { forest = forest'
                  , scanned = scanned' }

updateX :: ST s (Graph s) -> Vertex -> ST s (Graph s)
updateX graph x = do
    graph' <- graph
    return $ graph' { currentX = x }

updateY :: ST s (Graph s) -> Vertex -> ST s (Graph s)
updateY graph y = do
    graph' <- graph
    return $ graph' { currentY = y }

update :: ST s (Graph s) -> Property -> [(Vertex, Vertex)] -> ST s (Graph s)
update graph Ro xs = do
    graph' <- graph
    adjustHashTableFor xs $ (AF.ro . forest) graph'
    return graph'

updateSymmetric :: ST s (Graph s) -> Property -> [(Vertex, Vertex)] -> ST s (Graph s)
updateSymmetric graph Phi xs = do
    graph' <- graph
    adjustHashTableForSymmetric xs $ (AF.phi . forest) graph'
    return graph'
updateSymmetric graph Mu xs = do
    graph' <- graph
    adjustHashTableForSymmetric xs $ (AF.mu . forest) graph'
    return graph'

updateSingle :: ST s (Graph s) -> Property -> (Vertex, Vertex) -> ST s (Graph s)
updateSingle graph Phi (k, v) = do
    graph' <- graph
    adjustHashTable (k, v) $ (AF.phi . forest) graph'
    return graph'

updateScanned :: ST s (Graph s) -> (Vertex, Bool) -> ST s (Graph s)
updateScanned graph (k, v) = do
    graph' <- graph
    adjustHashTable (k, v) (scanned graph')
    return graph'

getVertex :: ST s (Graph s) -> Property -> Vertex -> ST s Vertex
getVertex graph property k = do
    graph' <- graph
    let lookupTable = case property of
            Mu  -> AF.mu (forest graph')
            Phi -> AF.phi (forest graph')
            Ro  -> AF.ro (forest graph')
    lookedUp <- HashTable.lookup lookupTable k
    case lookedUp of
        Just v -> return v
        Nothing -> return (-99)

getScanned :: ST s (Graph s) -> Vertex -> ST s Bool
getScanned graph k = do
    graph' <- graph
    lookedUp <- HashTable.lookup (scanned graph') k
    case lookedUp of
        Just flag -> return flag
        Nothing -> undefined


