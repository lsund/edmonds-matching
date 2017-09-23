
module Algorithm.Heuristics.MaximalMatching where

import Prelude ()
import Protolude
import Data.Graph.Core as Graph
import Data.List as List

maximalMatching :: Graph -> Matching
maximalMatching = maximalMatching' 1 [] []

maximalMatching' :: Vertex -> [Vertex] -> Matching -> Graph -> Matching
maximalMatching' x mvs matching graph =
    let nbs = neighbours graph x
        my = find (\y -> x `notElem` mvs && y `notElem` mvs) nbs
    in case my of
        Just y -> maximalMatching' x (x : y :  mvs) ((x, y) : matching) graph
        Nothing ->
            if succ x `elem` vertices graph then
                maximalMatching' (succ x) mvs matching graph
            else
                matching

matchingVertices :: Matching -> [Vertex]
matchingVertices =
    foldr (\(x, y) acc -> List.nub ([x, y] ++ acc)) []
