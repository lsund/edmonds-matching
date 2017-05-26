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
            Just (x, _) -> debugv "found root: " x $ findGrowth $ graph { currentX = x }
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
    let (px, py) = debugv "augmenting from: " (x, y) (pathToRoot graph x, pathToRoot graph y)
        (spx, spy) = (Set.fromList px, Set.fromList py)
    in 
        if areDisjoint spx spy
            then
                let (oddpx, oddpy) = odds px py
                    u = oddpx ++ oddpy
                    gu = map g u
                    keys = [x, y] ++ gu ++ u
                    vals = [y, x] ++ u ++ gu
                    forest' = 
                        (forest graph) { 
                            AF.mu = makeAssoc (adjustMapFor keys vals m) 
                        }
                    forest'' = AF.resetButMu 
                                    (representation graph) 
                                    forest'
                    graph' = graph { forest = forest'' }
                in findRoot graph'
            else shrink graph
    where
        x  = currentX graph
        y  = currentY graph
        nv = (length . vertices) graph
        ne = (length . edges) graph
        m  = (dict . AF.mu . forest) graph
        f  = (fun . AF.mu . forest) graph
        g  = (fun . AF.phi . forest) graph

shrink :: Graph -> Graph
shrink graph = 
    let (px, py) = debugvId "p(x), p(y): " (pathToRoot graph x, pathToRoot graph y)
        (spx, spy) = (Set.fromList px, Set.fromList py)
        isect    = spx `Set.intersection` spy
        r        = fromJust $ find (\x -> h x == x) isect
        (oddpx, oddpy) = odds px py
        union    =  spx `Set.union` spy
        oddUnion = Set.toList $ Set.fromList $ oddpx ++ oddpy
        filtered =  filter (\v -> (h . g) v /= r) oddUnion
        keys     = map g filtered
        vals     = filtered
        keys'    = appendIf (h x /= r) x keys
        vals'    = appendIf (h x /= r) y vals
        keys''   = appendIf (h y /= r) y keys'
        vals''   =  appendIf (h y /= r) x vals'
        xs       = debugvId "shrinking: " $ filter 
                    (\x -> h x `elem` union)
                    (vertices graph)
        forest'  = debugv "into: " r $ (forest graph) { AF.phi = 
                                     makeAssoc (adjustMapFor keys'' vals'' gm) 
                                  , AF.ro = 
                                     makeAssoc (adjustMapFor xs (repeat r) hm) }
    in findGrowth $ graph { forest = forest' }
    where
        x = currentX graph
        y = currentY graph
        gm = (dict . AF.phi . forest) graph
        g = (fun . AF.phi . forest) graph
        hm = (dict . AF.ro . forest) graph
        h = (fun . AF.ro . forest) graph


-- the edges are given as {x, mu(x)}
edmonds rep =
    let init = Graph.initialize rep
        graph = findRoot init
    in print $ matching graph

        -- gets (4, 8) this iteration. 


