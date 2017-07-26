
module Edmond.Algorithm.Heuristics where

import Edmond.Data.Graph as Graph
import Data.List as List

import Protolude

type Matching = [Edge]

maximalMatching :: ST s (Graph s) -> ST s Matching
maximalMatching = maximalMatching' 1 [] []

maximalMatching' :: Vertex -> [Vertex] -> Matching -> ST s (Graph s) -> ST s Matching
maximalMatching' x mvs matching graph = do
    graph' <- graph
    nbs <- neighbours graph' x
    let  my = find (\y -> x `notElem` mvs && y `notElem` mvs) nbs
    case my of
        Just y -> maximalMatching' x (x : y :  mvs) ((x, y) : matching) graph
        Nothing -> do
            graph' <- graph
            vs <- vertices graph'
            if succ x `elem` vs then
                maximalMatching' (succ x) mvs matching graph
            else
                return matching

matchingVertices :: Matching -> [Vertex]
matchingVertices = 
    foldr (\(x, y) acc -> List.nub ([x, y] ++ acc)) []
