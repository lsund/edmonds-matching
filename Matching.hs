
module Matching (edmonds) where

import Prelude hiding (lookup)
import Data.Graph
import Data.Array
import Data.Maybe
import Data.List hiding (lookup)
import qualified Data.Map as Map

type Map   = Map.Map
type VertexAssoc = Map Vertex Vertex
data State = State { mu      :: VertexAssoc
                   , phi     :: VertexAssoc
                   , ro      :: VertexAssoc
                   , scanned :: Map Vertex Bool }

applyEveryOther :: (a -> a) -> (a -> a) -> a -> [a]
applyEveryOther = applyEveryOther' True
    where
        applyEveryOther' True f g x = x : applyEveryOther' False f g (f x) 
        applyEveryOther' False f g x = x :  applyEveryOther' True f g (g x)

lookup :: VertexAssoc -> (Vertex -> Vertex)
lookup m x = fromJust $ Map.lookup x m 

p :: State -> Vertex -> [Vertex]
p (State f g _ _) = applyEveryOther (lookup f) (lookup g)

isOuter :: State -> Vertex -> Bool
isOuter (State f g _ _) v = fv == v || lookup g fv /= fv
    where fv = lookup f v 

isInner :: State -> Vertex -> Bool
isInner (State f g _ _) v = lookup g fv == fv && gv /= v
    where 
        fv  = lookup f v
        gv = lookup g v

isOutOfForest :: State -> Vertex -> Bool
isOutOfForest (State f g _ _) v = fv /= v && gv == v && gfv == fv
    where
        fv  = lookup f v
        gv = lookup g v
        gfv = lookup g fv

neighbours :: Graph -> Vertex -> [Vertex]
neighbours graph v = graph ! v

initializeState :: Graph -> State
initializeState graph = 
    let nv    = (length . vertices) graph
        ne    = (length . edges) graph
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in State idMap idMap idMap sInit

edmonds graph =
    let state = initializeState graph
        -- Map.assocs has RT O(n)
    in case find (\(_, y) -> not y) (Map.assocs (scanned state)) of 
            Nothing -> print "No unscanned outer edges. Terminating"
            Just (x, _) -> 
                let nbs = neighbours graph x
                in print nbs

