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
                     , scanned   :: ST s (HashTable s Vertex Bool)
                     , dimension :: (Int, Int)
                     , currentX  :: Vertex
                     , currentY  :: Vertex }

data Property = Mu | Phi | Ro
----------------------------------------------------------------------------
-- Initialize


initialize :: GraphRepresentation -> Graph s
initialize rep =
    let nv         = (length . Data.Graph.vertices) rep
        ne         = (length . Data.Graph.edges) rep
        sInit      = HashTable.new
        initForest = AF.initialize
    in Graph rep 
            (toBackward rep)
            initForest 
            sInit
            (nv, ne)
            (-1)
            (-1)
    where
        toBackward rep = 
            let edges = Data.Graph.edges rep
                redges = map swap edges
            in Data.Graph.buildG (1, length (Data.Graph.vertices rep)) redges


loadMatching :: Graph s -> [Edge] -> ST s (Graph s)
loadMatching graph = updateSymmetric graph Mu 

toMatching :: ST s (Graph s) -> ST s [Edge]
toMatching graph = do
    graph' <- graph
    mu <- (AF.mu . forest) graph'
    HashTable.foldM (\acc (k, v) -> 
        if k < v then return ((k, v) : acc)
        else return acc)
        [] mu

----------------------------------------------------------------------------
-- 'Usual' Graph properties

vertices :: Graph s -> [Vertex]
vertices = Data.Graph.vertices . forward

neighbours :: Graph s -> Vertex -> [Vertex]
neighbours graph v =
    let forw = forward graph
        backw = backward graph
    in (forw ! v) `List.union` (backw ! v)

----------------------------------------------------------------------------
-- API for AlternatingForest

reset :: Graph s -> Graph s
reset graph =
    let (nv, ne) = dimension graph
        scanned' = HashTable.new
        forest' = AF.reset (forest graph)
    in  graph { forest = forest',
               scanned = scanned' }

updateX :: Graph s -> Vertex -> Graph s
updateX graph x = graph { currentX = x }

updateY :: Graph s -> Vertex -> Graph s
updateY graph y = graph { currentY = y }

update :: Graph s -> Property -> [(Vertex, Vertex)] -> ST s (Graph s)
update graph Ro xs = do
    ro' <- AF.ro $ forest graph
    adjustHashTableFor xs ro'
    let forest' = (forest graph) { AF.ro = return ro' }
    return $ graph { forest = forest' }

updateSymmetric :: Foldable t
                => Graph s
                -> Property
                -> t (Vertex, Vertex)
                -> ST s (Graph s)
updateSymmetric graph Phi xs = do
    phi' <- AF.phi $ forest graph
    adjustHashTableForSymmetric xs phi'
    let forest' = (forest graph) { AF.phi = return phi' }
    return $ graph { forest = forest' }
updateSymmetric graph Mu xs = do
    mu' <- AF.mu $ forest graph
    adjustHashTableForSymmetric xs mu'
    return $ graph { forest = (forest graph) { AF.mu = return mu' } }

updateSingle :: Graph s -> Property -> (Vertex, Vertex) -> ST s (Graph s)
updateSingle graph Phi (k, v) = do
    phi' <- AF.phi $ forest graph
    adjustHashTable (k, v) phi'
    let forest' = (forest graph) { AF.phi = return phi' }
    return $ graph { forest = forest' }

updateScanned :: Graph s -> (Vertex, Bool) -> ST s (Graph s)
updateScanned graph (k, v) = do
    scanned' <- scanned graph
    adjustHashTable (k, v) scanned'
    return $ graph { scanned = return scanned' }

getVertex :: Graph s -> Property -> Vertex -> ST s Vertex
getVertex graph property k = do
    lookupTable <- case property of
            Mu  -> AF.mu (forest graph)
            Phi -> AF.phi (forest graph)
            Ro  -> AF.ro (forest graph)
    lookedUp <- HashTable.lookup lookupTable k
    case lookedUp of
        Just v -> return v
        Nothing -> return k

getScanned :: Graph s -> Vertex -> ST s Bool
getScanned graph k = do
    scanned' <- scanned graph
    lookedUp <- HashTable.lookup scanned' k
    case lookedUp of
        Just flag -> return flag
        Nothing -> return False

