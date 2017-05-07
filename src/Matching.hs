{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Matching where

import Protolude
import Data.Graph
import Data.Array
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe

type VertexAssoc = Map Vertex Vertex
data EdmondsState = EdmondsState { fa      :: VertexAssoc
                   , ga     :: VertexAssoc
                   , ra      :: VertexAssoc
                   , sa :: Map Vertex Bool } deriving (Show)

iterateEveryOther :: (a -> a) -> (a -> a) -> a -> [a]
iterateEveryOther = iterateEveryOther' True
    where
        iterateEveryOther' True f g x  = x : iterateEveryOther' False f g (f x)
        iterateEveryOther' False f g x = x :  iterateEveryOther' True f g (g x)

areDisjoint :: Ord a => Set a -> Set a -> Bool
areDisjoint xs = Set.null . Set.intersection xs

assocToFun :: VertexAssoc -> (Vertex -> Vertex)
assocToFun m x = fromJust $ Map.lookup x m 

pathToRoot :: EdmondsState -> Vertex -> [Vertex]
pathToRoot (EdmondsState f g _ _) v = v : takeWhile (/= v) (iterateFG v)
    where iterateFG = iterateEveryOther (assocToFun f) (assocToFun g)

isOuter :: EdmondsState -> Vertex -> Bool
isOuter (EdmondsState fa ga _ _) x = f x == x || g (f x) /= f x
    where f = assocToFun fa
          g = assocToFun ga

isInner :: EdmondsState -> Vertex -> Bool
isInner (EdmondsState fa ga _ _) x = g (f x) == f x && g x /= x
    where 
        f  = assocToFun fa
        g = assocToFun ga

isOutOfForest :: EdmondsState -> Vertex -> Bool
isOutOfForest (EdmondsState fa ga _ _) x = f x /= x && g x == x && g (f x)== f x
    where
        f  = assocToFun fa
        g = assocToFun ga

neighbours :: Graph -> Vertex -> [Vertex]
neighbours graph v = graph ! v

initializeState :: Graph -> EdmondsState
initializeState graph = 
    let nv    = (length . vertices) graph
        ne    = (length . edges) graph
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in EdmondsState idMap idMap idMap sInit

-- Map.assocs has RT O(n)
findXY :: Graph -> EdmondsState -> (Int, Int, EdmondsState)
findXY graph state@(EdmondsState fa ga ra sa) = 
    let mx = List.find (\(_, y) -> not y) (Map.assocs sa)
    in case mx of 
            Nothing -> undefined
            Just (x, _) -> 
                let pred y = isOutOfForest state y || 
                             isOuter state y && (assocToFun ga y /= assocToFun ga x)
                    my = List.find pred (neighbours graph x)
                in case my of
                    Nothing -> 
                        let sa' = Map.adjust (const True) x sa
                        in findXY graph (EdmondsState fa ga ra sa')
                    Just y -> (x, y, state)

grow :: Graph -> (Int, Int, EdmondsState) -> (Int, Int, EdmondsState)
grow graph (x, y, state@(EdmondsState fa ga ra sa)) = 
    if isOutOfForest state y
        then let ga' = Map.adjust (const x) y ga
             in  findXY graph (EdmondsState fa ga' ra sa)
        else (x, y, state)

-- augment :: Graph -> (Int, Int, EdmondsState) -> something???
augment graph (x, y, state) = 
    let px = Set.fromList $ pathToRoot state x 
        py = Set.fromList $ pathToRoot state y
    in areDisjoint px py

edmonds graph =
    let state = initializeState graph
        (x, y, state') = findXY graph state
        (x', y', state'') = grow graph (x, y, state')
    in augment graph (x', y', state'')

