{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm where

import Util
import Types
import Edmond.Data.Vertex
import Edmond.Data.Graph as Graph
import Edmond.Data.Assoc
import qualified Edmond.Data.AlternatingForest as AF

import Protolude
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Graph

odds px py = (every 2 px, every 2 py)

rootPaths graph x y = 
    let (px, py) = (pathToRoot graph x, pathToRoot graph y)
    in (px, py, Set.fromList px, Set.fromList py)
        -- converting to set to be able to call
        -- areDisjoint. Bad??

findRoot :: Graph -> (Edge, Graph)
findRoot graph =
    let mx = unscannedOuter graph (Map.assocs ((dict . scanned) graph))
    in case mx of 
            Nothing -> undefined
            Just (x, _) -> findGrowth graph x
    where
        unscannedOuter graph = List.find (\(x, y) -> not y && isOuter graph x)

-- Map.assocs has RT O(n)
-- At this point, we need to decide where to grow our tree.
-- Finds two vertices (x, y) such that x is an outer vertex with scanned(x) =
-- false, y is a neighbour of x such that y is out-of-forest or y is outer and
-- phi(x) =/ phi(y). (x, y) represents the edge, which we can use to grow the
-- tree
findGrowth :: Graph -> Vertex -> (Edge, Graph)
findGrowth graph x = 
    let pred' y = isOuter graph y && 
                       (fun . AF.ro . forest) graph y 
                    /= (fun . AF.ro . forest) graph x
        pred'' = isOutOfForest graph 
        pred y = pred'' y || pred' y
        my = List.find pred (neighbours (representation graph) x)
    in case my of
        Nothing -> 
            let f = const True
                scanned' = adjustMap x True $ (dict . scanned) graph
            in findRoot (graph { scanned = makeAssoc scanned' })
        Just y -> grow ((x, y), graph)

grow :: (Edge, Graph) -> (Edge, Graph)
grow ((x, y), graph)  = 
    if isOutOfForest graph y
        then let phi' = adjustMap y x m
                 forest' = (forest graph) { AF.phi = makeAssoc phi' }
            in findGrowth (graph { forest = forest' }) x
        else augment ((x, y), graph)
    where
        m = (dict . AF.phi . forest) graph

augment :: (Edge, Graph) -> (Edge, Graph)
augment ((x, y), graph) = 
    let (px, py, spx, spy) = rootPaths graph x y
    in 
        if areDisjoint spx spy
            then
                let (oddpx, oddpy) = odds px py
                    union = oddpx ++ oddpy
                    keys = [f x, f y] ++ map (f . g) union
                    vals = [y, x] ++ union
                    forest' = (forest graph) { AF.mu = 
                                        makeAssoc (adjustMapFor keys vals m) } 
                    forest'' = AF.resetButMu 
                                    (representation graph) 
                                    forest'
                    graph' = graph { forest = forest'' }
                in ((x, y), graph') 
            else
                undefined
    where
        nv = (length . vertices) graph
        ne = (length . edges) graph
        m = (dict . AF.mu . forest) graph
        f = (fun . AF.mu . forest) graph
        g = (fun . AF.phi . forest) graph

shrink ((x, y), graph) = 
    let (px, py, spx, spy) = rootPaths graph x y
        isect    = spx `Set.intersection` spy
        r        = fromJust $ find (\x -> h x == x) isect
        (oddpx, oddpy) = odds px py
        union    = oddpx ++ oddpy
        union'   = filter (\v -> (h . g) v /= r) union
        keys     = map (g . g) union'
        vals     = union'
        keys'     = appendIf (h x /= r) (g y) keys
        vals'    = appendIf (h y /= r) y vals
        keys''   = appendIf (h y /= r) (g y) keys'
        vals''   = appendIf (h y /= r) x vals'
        xs       = filter 
                    (\x -> inUnion (h x) spx spy)
                    (Data.Graph.vertices (representation graph'))
        forest'  = (forest graph) { AF.phi = 
                                     makeAssoc (adjustMapFor keys'' vals'' m) 
                                  , AF.ro = 
                                    makeAssoc (adjustMapFor xs (repeat r) m) }
        graph'   = graph { forest = forest' }
    in r
    where
        m = (dict . AF.phi . forest) graph
        h = (fun . AF.ro . forest) graph
        g = (fun . AF.phi . forest) graph
        inUnion x spx spy = x `Set.member` (spx `Set.union` spy)


-- the edges are given as {x, mu(x)}
edmonds rep =
    let init = Graph.initialize rep
        (es, graph) = findRoot graph
    in matching graph

        -- gets (4, 8) this iteration. 


