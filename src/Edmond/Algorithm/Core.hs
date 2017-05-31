{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Edmond.Algorithm.Core where

import Util
import Edmond.Data.Graph as Graph
import Edmond.Data.Assoc
import qualified Edmond.Data.AlternatingForest as AF
import Edmond.Algorithm.Helpers

import Protolude
import Data.Maybe
import qualified Data.Graph
import qualified Data.List as List

-- Finds an x such that x is not scanned and x is outer. If success, proceed
-- with calling findGrowth with the found x. If unsuccessful, return the graph
-- of the current state.
findRoot :: Graph -> Graph
findRoot graph =
    let mx = find (\x -> all ($ x) [not . isScanned graph, isOuter graph]) vs
    in case mx of 
            Nothing -> graph
            Just x -> findGrowth $ graph { currentX = x }
    where vs = vertices graph
            

-- Given a graph and a vertex x, finds a neighbour y of x such that y is either
-- out-of-forest or (y is outer and ro(y) =/ ro(x)
findNeighbour :: Graph -> Maybe Vertex
findNeighbour graph =
    let pred' y = isOuter graph y && 
                       (fun . AF.ro . forest) graph y 
                    /= (fun . AF.ro . forest) graph x
        pred'' = isOutOfForest graph
        pred y = pred'' y || pred' y
        found = find pred $ neighbours (representation graph) x
    in found
    where
        x = currentX graph

-- Map.assocs has RT O(n)
-- At this point, we need to decide where to grow our tree.
-- Finds two vertices (x, y) such that x is an outer vertex with scanned(x) =
-- false, y is a neighbour of x such that y is out-of-forest or y is outer and
-- phi(x) =/ phi(y). (x, y) represents the edge, which we can use to grow the
-- tree
findGrowth :: Graph -> Graph
findGrowth graph = 
    case findNeighbour graph of
        Nothing -> 
            let f = const True
                scanned' = adjustMap x True $ (dict . scanned) graph
            in findRoot (graph { scanned = makeAssoc scanned' })
        Just y -> grow (graph { currentY = y })
    where
        x = currentX graph

grow :: Graph -> Graph
grow graph = 
    if isOutOfForest graph y
        then 
            let phi' = adjustMap y x m
                forest' = (forest graph) { AF.phi = makeAssoc phi' }
            in findGrowth (graph { forest = forest' })
        else augment graph
    where
        m = (dict . AF.phi . forest) graph
        x = currentX graph
        y = currentY graph

augment :: Graph -> Graph
augment graph = 
    let (px, py) = (pathToRoot graph x, pathToRoot graph y)
    in if areDisjoint px py
        then
            let (oddpx, oddpy) = odds px py
                u = oddpx ++ oddpy
                gu = map g u
                m' = adjustMapFor ([x, y] ++ gu) ([y, x] ++ u) $ adjustMapFor u gu m
                forest'' = AF.resetButMu (representation graph) m'
                graph' = graph { forest = forest'' }
            in findRoot graph'
        else shrink px py graph
    where
        x  = currentX graph
        y  = currentY graph
        nv = (length . vertices) graph
        ne = (length . edges) graph
        m  = (dict . AF.mu . forest) graph
        g  = (fun . AF.phi . forest) graph

shrink :: [Vertex] ->
          [Vertex] ->
          Graph ->
          Graph
shrink px py graph = 
    let isect    = px `List.intersect` py
        r        = fromJust $ find (\x -> h x == x) isect
        (pxr, pyr) = (takeUntil r px, takeUntil r py)
        (oddpx, oddpy) = odds pxr pyr
        union    = pxr `List.union` pyr
        oddUnion = oddpx ++ oddpy
        filtered = filter (\v -> (h . g) v /= r) oddUnion
        keys     = map g filtered
        vals     = filtered
        gm'      = adjustMapFor keys vals gm
        gm''     = if h x /= r then adjustMap x y gm' else gm'
        gm'''    = if h y /= r then adjustMap y x gm'' else gm''
        keys'    = filter (\x -> h x `elem` union) (vertices graph)
        hm'      = adjustMapFor keys' (replicate (length keys') r) hm
        forest'  = (forest graph) { AF.phi = makeAssoc gm'''
                                  , AF.ro = makeAssoc hm'
                                  }
    in findGrowth $ graph { forest = forest' }
    where
        x = currentX graph
        y = currentY graph
        gm = (dict . AF.phi . forest) graph
        g = (fun . AF.phi . forest) graph
        hm = (dict . AF.ro . forest) graph
        h = (fun . AF.ro . forest) graph


edmonds :: Data.Graph.Graph -> IO [Edge]
edmonds rep =
    let init = Graph.initialize rep
        graph = findRoot init
    in return $ matching graph

