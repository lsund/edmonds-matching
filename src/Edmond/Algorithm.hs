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

-- augment :: Graph -> (Edge, State) -> something???
augment graph ((x, y), state) = 
    let px = Set.fromList $ pathToRoot state x 
        py = Set.fromList $ pathToRoot state y
    in 
        if areDisjoint px py 
            then
                let pUnion = Set.toList $ px `Set.union` py
                in adjustMapFor (map (muf . phif) pUnion) pUnion mud
        else undefined
    where
        muf = (fun . mu) state
        mud = (dict . mu) state
        phif = (fun . phi) state


edmonds graph =
    let state = State.initialize graph
        ((x, y), state') = findGrowth graph state
        ((x', y'), state'') = grow graph ((x, y), state')
    in augment graph ((x', y'), state'')

