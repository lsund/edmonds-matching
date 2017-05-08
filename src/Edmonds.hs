{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Edmonds where

import Edmond.Vertex
import Edmond.State as State
import Graph
import Protolude hiding (State)

import Data.Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

areDisjoint :: Ord a => Set a -> Set a -> Bool
areDisjoint xs = Set.null . Set.intersection xs

-- Map.assocs has RT O(n)
-- At this point, we need to decide where to grow our tree.
-- Finds two vertices (x, y) such that x is an outer vertex with scanned(x) =
-- false, y is a neighbour of x such that y is out-of-forest or y is outer and
-- phi(x) =/ phi(y)
findGrowth :: Graph -> State -> (Int, Int, State)
findGrowth graph state = 
    let mx = unscanned (Map.assocs (scanned state))
    in case mx of 
            Nothing -> undefined -- Stop if reached here
            Just (x, _) -> 
                let pred y = isOutOfForest state y || 
                             isOuter state y && (fun (phi state) y /= fun (phi state ) x)
                    my = List.find pred (neighbours graph x)
                in case my of
                    Nothing -> 
                        let f = const True
                            scanned' = Map.adjust f x (scanned state)
                        in findGrowth graph $ state { scanned = scanned' }
                    Just y -> (x, y, state)
    where
        unscanned = List.find (\(_, y) -> not y)

grow :: Graph -> (Int, Int, State) -> (Int, Int, State)
grow graph (x, y, state) = 
    if isOutOfForest state y
        then let phi' = Map.adjust (const x) y (State.map (phi state))
             in  findGrowth graph $ state { phi = VertexAssoc phi' (assocToFun phi') }
        else (x, y, state)

-- augment :: Graph -> (Int, Int, State) -> something???
augment graph (x, y, state) = 
    let px = Set.fromList $ pathToRoot state x 
        py = Set.fromList $ pathToRoot state y
    in areDisjoint px py

edmonds graph =
    let state = State.initialize graph
        (x, y, state') = findGrowth graph state
        (x', y', state'') = grow graph (x, y, state')
    in augment graph (x', y', state'')

