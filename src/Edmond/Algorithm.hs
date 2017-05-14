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

-- Map.assocs has RT O(n)
-- At this point, we need to decide where to grow our tree.
-- Finds two vertices (x, y) such that x is an outer vertex with scanned(x) =
-- false, y is a neighbour of x such that y is out-of-forest or y is outer and
-- phi(x) =/ phi(y). (x, y) represents the edge, which we can use to grow the
-- tree
findGrowth :: Graph -> State -> (Edge, State)
findGrowth graph state = 
    let mx = unscanned (Map.assocs ((dict . scanned) state))
    in case mx of 
            Nothing -> undefined -- Stop if reached here
            Just (x, _) -> 
                let pred' y = isOuter state y && 
                              (fun . ro) state y /= (fun . ro) state  x
                    pred'' = isOutOfForest state 
                    pred y = pred'' y || pred' y
                    my = List.find pred (neighbours graph x)
                in case my of
                    Nothing -> 
                        let f = const True
                            scanned' = adjustMap x True $ (dict . scanned) state
                        in findGrowth graph $ 
                                      state { scanned = makeAssoc scanned' }
                    Just y -> ((x, y), state)
    where
        unscanned = List.find (\(_, y) -> not y)

grow :: Graph -> (Edge, State) -> (Edge, State)
grow graph ((x, y), state) = 
    if isOutOfForest state y
        then let phi' = adjustMap y x $ (dict . phi) state 
             in  findGrowth graph $ state { phi = makeAssoc phi' }
        else ((x, y), state)

rootPaths state x y = 
    let (px, py) = (pathToRoot state x, pathToRoot state y)
    in (px, py, Set.fromList px, Set.fromList py)
        -- converting to set to be able to call
        -- areDisjoint. Bad??

odds px py = (every 2 px, every 2 py)

augment :: Graph -> (Edge, State) -> State
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
                in State.resetButMu (lv graph) (le graph) state'
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
        keys'    = if h x /= r then g x : keys else keys
        keys''   = if h y /= r then g y : keys' else keys'
        vals'    = if h x /= r then y   : vals else vals
        vals''   = if h y /= r then x   : vals' else vals'
        state'   = state { phi = makeAssoc (adjustMapFor keys'' vals'' m) }
        xs       = filter (\x -> inUnion (h x) spx spy) (Graph.vertices graph)
        state''  = state' { ro = makeAssoc (adjustMapFor xs (repeat r) m) }
    in r
    where
        m = (dict . phi) state
        h = (fun . ro) state
        g = (fun . phi) state
        inUnion x spx spy = x `Set.member` (spx `Set.union` spy)

-- the edges are given as {x, mu(x)}
edmonds graph =
    let lv = length . Graph.vertices
        le = length . Graph.edges
        state = State.initialize (lv graph) (le graph)
        ((x, y), state') = findGrowth graph state
        ((x', y'), state'') = grow graph ((x, y), state')
        augmented =  augment graph ((x', y'), state'')
    in ((dict . mu) augmented, (dict . mu) state)

