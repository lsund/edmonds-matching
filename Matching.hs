
module Matching (edmonds) where

import Prelude hiding (lookup)
import Data.Graph
import Data.Maybe
import qualified Data.Map as Map

type Map   = Map.Map
data State = State { mu      :: Map Int Int
                   , phi     :: Map Int Int
                   , ro      :: Map Int Int
                   , scanned :: Map Int Int }

applyEveryOther :: (a -> a) -> (a -> a) -> a -> [a]
applyEveryOther = applyEveryOther' True
    where
        applyEveryOther' True f g x = x : applyEveryOther' False f g (f x) 
        applyEveryOther' False f g x = x :  applyEveryOther' True f g (g x)

lookup :: Map Int Int -> (Int -> Int)
lookup m x = fromJust $ Map.lookup x m 

p :: State -> Int -> [Int]
p (State f g _ _) = applyEveryOther (lookup f) (lookup g)

isOuter :: State -> Int -> Bool
isOuter (State f g _ _) v = fv == v || lookup g fv /= fv
    where fv = lookup f v 

isInner :: State -> Int -> Bool
isInner (State f g _ _) v = lookup g fv == fv && gv /= v
    where 
        fv  = lookup f v
        gv = lookup g v

isOutOfForest :: State -> Int -> Bool
isOutOfForest (State f g _ _) v = fv /= v && gv == v && gfv == fv
    where
        fv  = lookup f v
        gv = lookup g v
        gfv = lookup g fv
        
edmonds graph =
    let nv    = (length . vertices) graph
        ne    = (length . edges) graph
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
        fInit = idMap
        gInit = idMap
        hInit = idMap
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate 10 False]
    in
        fInit

