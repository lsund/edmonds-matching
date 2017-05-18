{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm where

import Util
import Types
import Edmond.Vertex
import Edmond.State as State
import Edmond.Assoc

import Protolude hiding (State)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Graph as Graph

odds px py = (every 2 px, every 2 py)

rootPaths state x y = 
    let (px, py) = (pathToRoot state x, pathToRoot state y)
    in (px, py, Set.fromList px, Set.fromList py)
        -- converting to set to be able to call
        -- areDisjoint. Bad??

findRoot :: Graph -> State -> (Edge, State)
findRoot graph state =
    let mx = unscannedOuter state (Map.assocs ((dict . scanned) state))
    in case mx of 
            Nothing -> undefined
            Just (x, _) -> findGrowth graph state x
    where
        unscannedOuter state = List.find (\(x, y) -> not y && isOuter state x)

-- Map.assocs has RT O(n)
-- At this point, we need to decide where to grow our tree.
-- Finds two vertices (x, y) such that x is an outer vertex with scanned(x) =
-- false, y is a neighbour of x such that y is out-of-forest or y is outer and
-- phi(x) =/ phi(y). (x, y) represents the edge, which we can use to grow the
-- tree
findGrowth :: Graph -> State -> Vertex -> (Edge, State)
findGrowth graph state x = 
    let pred' y = isOuter state y && 
                    (fun . ro) state y /= (fun . ro) state x
        pred'' = isOutOfForest state 
        pred y = pred'' y || pred' y
        my = List.find pred (neighbours graph x)
    in case my of
        Nothing -> 
            let f = const True
                scanned' = adjustMap x True $ (dict . scanned) state
            in findRoot graph (state { scanned = makeAssoc scanned' })
        Just y -> grow graph ((x, y), state)

grow :: Graph -> (Edge, State) -> (Edge, State)
grow graph ((x, y), state)  = 
    if isOutOfForest state y
        then let phi' = adjustMap y x $ (dict . phi) state
            in findGrowth graph (state { phi = makeAssoc phi' }) x
        else augment graph ((x, y), state)

augment :: Graph -> (Edge, State) -> (Edge, State)
augment graph ((x, y), state) = 
    let (px, py, spx, spy) = rootPaths state x y
    in 
        if areDisjoint spx spy
            then
                let (oddpx, oddpy) = odds px py
                    union = oddpx ++ oddpy
                    keys = [f x, f y] ++ map (f . g) union
                    vals = [y, x] ++ union
                    state' = state { mu = makeAssoc (adjustMapFor keys vals m) }
                in ((x, y), State.resetButMu (lv graph) (le graph) state')
            else
                undefined
    where
        m = (dict . mu) state
        f = (fun . mu) state
        g = (fun . phi) state
        lv = length . Graph.vertices
        le = length . Graph.edges

shrink graph ((x, y), state) = 
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
        xs       = filter (\x -> inUnion (h x) spx spy) (Graph.vertices graph)
        state''  = state' { ro = makeAssoc (adjustMapFor xs (repeat r) m) }
    in r
    where
        m = (dict . phi) state
        h = (fun . ro) state
        g = (fun . phi) state
        inUnion x spx spy = x `Set.member` (spx `Set.union` spy)


-- loop graph state 3 xs = (xs, state)
-- loop graph state i xs = 
--     case grow graph state of
--         Nothing -> undefined
--         Just ((x, y), state') -> 
--             loop graph (augment graph ((x, y), state')) (succ i) ((x, y) : xs)


-- the edges are given as {x, mu(x)}
edmonds graph =
    let lv = length . Graph.vertices
        le = length . Graph.edges
        init = State.initialize (lv graph) (le graph)
        (es, state) = findRoot graph state
    -- in (outers graph state,
    --     inners graph state,
    --     outOfForests graph state,
    --     reverse es)
    in matching state

        -- gets (4, 8) this iteration. 


