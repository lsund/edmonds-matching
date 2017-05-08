{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm where

import Util
import Edmond.Vertex
import Edmond.State as State
import Graph
import Protolude hiding (State)

import Data.Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

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
                              (fun . phi) state y /= (fun . phi) state  x
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

augment :: Graph -> (Edge, State) -> State
augment graph ((x, y), state) = 
    let px = pathToRoot state x
        py = pathToRoot state y
        spx = Set.fromList px       -- converting to set to be able to call
        spy = Set.fromList py       -- areDisjoint. Bad??
    in 
        if areDisjoint spx spy
            then
                let oddxs = every 2 px
                    oddys = every 2 py
                    pUnion = oddxs ++ oddys
                    keys = [f x, f y] ++ map (f . g) pUnion
                    vals = [y, x] ++ pUnion
                    state' = state { mu = makeAssoc (adjustMapFor keys vals m) }
                in State.resetButMu (lv graph) (le graph) state'
            else
                undefined
    where
        m = (dict . mu) state
        f = (fun . mu) state
        g = (fun . phi) state
        lv = length . vertices
        le = length . edges

edmonds graph =
    let lv = length . vertices
        le = length . edges
        state = State.initialize (lv graph) (le graph)
        ((x, y), state') = findGrowth graph state
        ((x', y'), state'') = grow graph ((x, y), state')
    in augment graph ((x', y'), state'')

