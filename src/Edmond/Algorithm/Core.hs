{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Edmond.Algorithm.Core where

import Logger
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

findRoot :: Graph -> (Edge, Graph)
findRoot graph =
    let mx = unscannedOuter graph (Map.assocs ((dict . scanned) graph))
    in case mx of 
            Nothing -> ((-1, -1), graph) -- TODO change to something sensible
            Just (x, _) -> 
                findGrowth (Graph.log (appendShow "Found root: " x) graph) x
    where
        unscannedOuter graph = find (\(x, y) -> not y && isOuter graph x)

-- Given a graph and a vertex x, finds a neighbour y of x such that y is either
-- out-of-forest or (y is outer and ro(y) =/ ro(x)
findNeighbour :: Graph -> Vertex -> Maybe Vertex
findNeighbour graph x =
    let pred' y = isOuter graph y && 
                       (fun . AF.ro . forest) graph y 
                    /= (fun . AF.ro . forest) graph x
        pred'' = isOutOfForest graph 
        pred y = pred'' y || pred' y
    in find pred (neighbours (representation graph) x)

-- Map.assocs has RT O(n)
-- At this point, we need to decide where to grow our tree.
-- Finds two vertices (x, y) such that x is an outer vertex with scanned(x) =
-- false, y is a neighbour of x such that y is out-of-forest or y is outer and
-- phi(x) =/ phi(y). (x, y) represents the edge, which we can use to grow the
-- tree
findGrowth :: Graph -> Vertex -> (Edge, Graph)
findGrowth graph x = 
    case findNeighbour graph x of
        Nothing -> 
            let f = const True
                logged = Graph.log "No growth found" graph
                scanned' = adjustMap x True $ (dict . scanned) logged
            in findRoot (logged { scanned = makeAssoc scanned' })
        Just y -> 
            let logged = Graph.log (appendShow "Found growth: " (x, y)) graph
            in grow ((x, y), logged)

grow :: (Edge, Graph) -> (Edge, Graph)
grow ((x, y), graph)  = 
    if isOutOfForest graph y
        then 
            let phi' = adjustMap y x m
                forest' = (forest graph) { AF.phi = makeAssoc phi' }
            in findGrowth (graph { forest = forest' }) x
        else
            let logged = Graph.log (appendShow "Grew tree : " (x, y)) graph
            in augment ((x, y), logged)
    where
        m = (dict . AF.phi . forest) graph

augment :: (Edge, Graph) -> (Edge, Graph)
augment ((x, y), graph) = 
    let (px, py, spx, spy) = rootPaths logged x y
        logged = Graph.log (appendShow "px " px) $ 
                    Graph.log (appendShow "mu x " (f x)) graph
    in 
        if areDisjoint spx spy
            then
                let (oddpx, oddpy) = odds px py
                    union = oddpx ++ oddpy
                    keys = [f x, f y] ++ map (f . g) union
                    vals = [y, x] ++ union
                    forest' = 
                        (forest logged) { 
                            AF.mu = makeAssoc (adjustMapFor keys vals m) 
                        }
                    forest'' = AF.resetButMu 
                                    (representation logged) 
                                    forest'
                    graph' = logged { forest = forest'' }
                    logged' = Graph.log (appendShow "y root path: " py) $
                                Graph.log (appendShow "x root path: " px) $
                                Graph.log "Augmented" graph' 
                in findRoot logged'
            else
                findGrowth logged x
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
                    (vertices graph')
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
        ((x, y), graph) = findRoot init
        logged = Graph.log (appendShow "matching: " (matching graph)) graph
    in putStrLn $ Logger.read (logger logged) 

        -- gets (4, 8) this iteration. 


