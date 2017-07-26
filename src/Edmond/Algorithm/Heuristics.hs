
module Edmond.Algorithm.Heuristics where

import Edmond.Data.Graph as Graph
import Data.List as List

import Protolude

type Matching = [Edge]

maximalMatching :: Graph s -> Matching
maximalMatching = maximalMatching' 1 [] []

maximalMatching' :: Vertex -> [Vertex] -> Matching -> Graph s -> Matching
maximalMatching' x mvs matching graph =
    let nbs = neighbours graph x
        my = find (\y -> x `notElem` mvs && y `notElem` mvs) nbs
    in case my of
        Just y -> maximalMatching' x (x : y :  mvs) ((x, y) : matching) graph
        Nothing ->
            let vs = vertices graph
            in 
                if succ x `elem` vs then
                    maximalMatching' (succ x) mvs matching graph
                else
                    matching

matchingVertices :: Matching -> [Vertex]
matchingVertices = 
    foldr (\(x, y) acc -> List.nub ([x, y] ++ acc)) []
