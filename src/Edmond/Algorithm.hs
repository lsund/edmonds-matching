{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm where

import Util
import Types
import Edmond.Vertex
import Edmond.Graph as Graph
import Edmond.Assoc

import Protolude
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Graph

odds px py = (every 2 px, every 2 py)

rootPaths state x y = 
    let (px, py) = (pathToRoot state x, pathToRoot state y)
    in (px, py, Set.fromList px, Set.fromList py)
        -- converting to set to be able to call
        -- areDisjoint. Bad??

findRoot :: Graph -> (Edge, Graph)
findRoot state =
    let mx = unscannedOuter state (Map.assocs ((dict . scanned) state))
    in case mx of 
            Nothing -> undefined
            Just (x, _) -> findGrowth state x
    where
        unscannedOuter state = List.find (\(x, y) -> not y && isOuter state x)

-- Map.assocs has RT O(n)
-- At this point, we need to decide where to grow our tree.
-- Finds two vertices (x, y) such that x is an outer vertex with scanned(x) =
-- false, y is a neighbour of x such that y is out-of-forest or y is outer and
-- phi(x) =/ phi(y). (x, y) represents the edge, which we can use to grow the
-- tree
findGrowth :: Graph -> Vertex -> (Edge, Graph)
findGrowth state x = 
    let pred' y = isOuter state y && 
                    (fun . ro) state y /= (fun . ro) state x
        pred'' = isOutOfForest state 
        pred y = pred'' y || pred' y
        my = List.find pred (neighbours (graph state) x)
    in case my of
        Nothing -> 
            let f = const True
                scanned' = adjustMap x True $ (dict . scanned) state
            in findRoot (state { scanned = makeAssoc scanned' })
        Just y -> grow ((x, y), state)

grow :: (Edge, Graph) -> (Edge, Graph)
grow ((x, y), state)  = 
    if isOutOfForest state y
        then let phi' = adjustMap y x $ (dict . phi) state
            in findGrowth (state { phi = makeAssoc phi' }) x
        else augment ((x, y), state)

augment :: (Edge, Graph) -> (Edge, Graph)
augment ((x, y), state) = 
    let (px, py, spx, spy) = rootPaths state x y
    in 
        if areDisjoint spx spy
            then
                let (oddpx, oddpy) = odds px py
                    union = oddpx ++ oddpy
                    keys = [f x, f y] ++ map (f . g) union
                    vals = [y, x] ++ union
                    state' = state { mu = makeAssoc (adjustMapFor keys vals m) }
                in ((x, y), Graph.resetButMu (lv (graph state)) (le (graph state)) state')
            else
                undefined
    where
        m = (dict . mu) state
        f = (fun . mu) state
        g = (fun . phi) state
        lv = length . Data.Graph.vertices
        le = length . Data.Graph.edges

shrink ((x, y), state) = 
    let (px, py, spx, spy) = rootPaths state x y
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
        state'   = state { phi = makeAssoc (adjustMapFor keys'' vals'' m) }
        xs       = filter 
                    (\x -> inUnion (h x) spx spy)
                    (Data.Graph.vertices (graph state'))
        state''  = state' { ro = makeAssoc (adjustMapFor xs (repeat r) m) }
    in r
    where
        m = (dict . phi) state
        h = (fun . ro) state
        g = (fun . phi) state
        inUnion x spx spy = x `Set.member` (spx `Set.union` spy)


-- the edges are given as {x, mu(x)}
edmonds graph =
    let lv = length . Data.Graph.vertices
        le = length . Data.Graph.edges
        init = Graph.initialize graph (lv graph) (le graph)
        (es, state) = findRoot state
    -- in (outers graph state,
    --     inners graph state,
    --     outOfForests graph state,
    --     reverse es)
    in matching state

        -- gets (4, 8) this iteration. 


