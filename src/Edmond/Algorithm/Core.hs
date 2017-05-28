{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Edmond.Algorithm.Core where

import Util
import Edmond.Data.Vertex
import Edmond.Data.Graph as Graph
import Edmond.Data.Assoc
import qualified Edmond.Data.AlternatingForest as AF
import Edmond.Algorithm.Helpers

import Protolude
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text 

(+++) :: Text -> Text -> Text
a +++ b = a `Data.Text.append` b

debug :: Text -> a -> a
debug = trace

debugv :: Show a => Text -> a -> b -> b
debugv msg val = trace (msg +++ show val) 

debugvId :: Show a => Text -> a -> a
debugvId msg expr = trace (msg +++ show expr) expr

findRoot :: Graph -> Graph
findRoot graph =
    let mx = unscannedOuter graph (Map.assocs ((dict . scanned) graph))
    in case mx of 
            Nothing -> graph
            Just (x, _) -> findGrowth $ graph { currentX = x }
    where
        unscannedOuter graph = find (\(x, y) -> not y && isOuter graph x)

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
        (spx, spy) = (Set.fromList px, Set.fromList py)
    in 
        if areDisjoint spx spy
            then
                let (oddpx, oddpy) = odds px py
                    u = oddpx ++ oddpy
                    gu = map g u
                    m' = adjustMapFor gu u m
                    m'' = adjustMapFor u gu m'
                    m''' = adjustMapFor [x, y] [y, x] m''
                    forest'' = AF.resetButMu (representation graph) m'''
                    graph' = graph { forest = forest'' }
                in findRoot graph'
            else shrink px py spx spy graph
    where
        x  = currentX graph
        y  = currentY graph
        nv = (length . vertices) graph
        ne = (length . edges) graph
        m  = (dict . AF.mu . forest) graph
        f  = (fun . AF.mu . forest) graph
        g  = (fun . AF.phi . forest) graph

shrink :: [Vertex] ->
          [Vertex] ->
          Set.Set Vertex ->
          Set.Set Vertex ->
          Graph ->
          Graph
shrink px py spx spy graph = 
    let isect    = spx `Set.intersection` spy
        r        = fromJust $ find (\x -> h x == x) isect
        (pxr, pyr) = (takeUntil r px, takeUntil r py)
        (spxr, spyr) = (Set.fromList pxr, Set.fromList pyr) 
        (oddpx, oddpy) = odds pxr pyr
        union    = spxr `Set.union` spyr
        oddUnion = oddpx ++ oddpy
        filtered =  filter (\v -> (h . g) v /= r) oddUnion
        keys     = map g filtered
        vals     = filtered
        gm'      = adjustMapFor keys vals gm
        gm''     = if h x /= r then adjustMap x y gm' else gm'
        gm'''    = if h y /= r then adjustMap y x gm'' else gm''
        keys'    = filter (\x -> h x `elem` union) (vertices graph)
        hm'      = adjustMapFor keys' (repeat r) hm
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


edmonds rep =
    let init = Graph.initialize rep
        graph = findRoot init
    in print $ length $ matching graph


