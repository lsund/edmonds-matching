
module Matching where

import Prelude hiding (lookup)
import Data.Graph
import Data.Array
import Data.Maybe
import Data.List hiding (lookup)
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

type Map   = Map.Map
type Set   = Set.Set
type VertexAssoc = Map Vertex Vertex
data State = State { fa      :: VertexAssoc
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

lookup :: VertexAssoc -> (Vertex -> Vertex)
lookup m x = fromJust $ Map.lookup x m 

pathToRoot :: State -> Vertex -> [Vertex]
pathToRoot (State f g _ _) v = v : takeWhile (/= v) (iterateFG v)
    where iterateFG = iterateEveryOther (lookup f) (lookup g)

isOuter :: State -> Vertex -> Bool
isOuter (State fa ga _ _) x = f x == x || g (f x) /= f x
    where f = lookup fa
          g = lookup ga

isInner :: State -> Vertex -> Bool
isInner (State fa ga _ _) x = g (f x) == f x && g x /= x
    where 
        f  = lookup fa
        g = lookup ga

isOutOfForest :: State -> Vertex -> Bool
isOutOfForest (State fa ga _ _) x = f x /= x && g x == x && g (f x)== f x
    where
        f  = lookup fa
        g = lookup ga

neighbours :: Graph -> Vertex -> [Vertex]
neighbours graph v = graph ! v

initializeState :: Graph -> State
initializeState graph = 
    let nv    = (length . vertices) graph
        ne    = (length . edges) graph
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in State idMap idMap idMap sInit

-- Map.assocs has RT O(n)
findXY :: Graph -> State -> (Int, Int, State)
findXY graph state@(State fa ga ra sa) = 
    let mx = find (\(_, y) -> not y) (Map.assocs sa)
    in case mx of 
            Nothing -> undefined
            Just (x, _) -> 
                let pred y = isOutOfForest state y || 
                             isOuter state y && (lookup ga y /= lookup ga x)
                    my = find pred (neighbours graph x)
                in case my of
                    Nothing -> 
                        let sa' = Map.adjust (const True) x sa
                        in findXY graph (State fa ga ra sa')
                    Just y -> (x, y, state)

grow :: Graph -> (Int, Int, State) -> (Int, Int, State)
grow graph (x, y, state@(State fa ga ra sa)) = 
    if isOutOfForest state y
        then let ga' = Map.adjust (const x) y ga
             in  findXY graph (State fa ga' ra sa)
        else (x, y, state)

-- augment :: Graph -> (Int, Int, State) -> something???
augment graph (x, y, state) = 
    let px = Set.fromList $ pathToRoot state x 
        py = Set.fromList $ pathToRoot state y
    in areDisjoint px py

edmonds graph =
    let state = initializeState graph
        (x, y, state') = findXY graph state
        (x', y', state'') = grow graph (x, y, state')
    in augment graph (x', y', state'')

