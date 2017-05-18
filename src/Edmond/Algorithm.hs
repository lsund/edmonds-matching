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
findGrowth :: Graph -> State -> Maybe (Edge, State)
findGrowth graph state = 
    let mx = unscanned state (Map.assocs ((dict . scanned) state))
    in case mx of 
            Nothing -> Nothing
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
                    Just y -> Just ((x, y), state)
    where
        unscanned state = List.find (\(x, y) -> not y && isOuter state x)

grow :: Graph -> State -> Maybe (Edge, State)
grow graph state = 
    case findGrowth graph state of 
        Nothing -> Nothing
        Just ((x, y), state) ->
            if isOutOfForest state y
                then let phi' = adjustMap y x $ (dict . phi) state 
                    in  findGrowth graph $ state { phi = makeAssoc phi' }
                else Just ((x, y), state)

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

-- the edges are given as {x, mu(x)}
edmonds graph =
    let lv = length . Graph.vertices
        le = length . Graph.edges
        init = State.initialize (lv graph) (le graph)
    in 
        case grow graph init of
            Nothing -> undefined
            Just ((x, y), grown1) ->
                let augmented1 = augment graph ((x, y), grown1)
                in case grow graph augmented1 of
                    Nothing -> undefined
                    Just ((x2, y2), grown2) -> 
                        let augmented2 = augment graph ((x2, y2), grown2)
                        in case grow graph augmented2 of
                            Nothing -> undefined
                            Just ((x3, y3), grown3) ->
                                let augmented3 = augment graph ((x3, y3), grown3)
                                in matching augmented3
                                -- in case grow graph augmented3 of
                                --     Nothing -> undefined
                                --     Just ((x4, y4), grown4) -> 
                                --         let augmented4 = augment graph ((x4, y4), grown4)
                                --         in matching augmented4
                                --         -- in case grow graph augmented4 of
                                --         -- [(x, y), (x2, y2), (x3, y3), (x4, y4)]


