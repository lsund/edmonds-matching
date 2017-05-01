
module Matching (edmonds) where

import Prelude hiding (lookup)
import Data.Graph
import Data.Maybe
import qualified Data.Map as Map

type Map   = Map.Map
type State = (Map Int Int, Map Int Int, Map Int Int)

applyEveryOther :: (a -> a) -> (a -> a) -> a -> [a]
applyEveryOther = applyEveryOther' True
    where
        applyEveryOther' True f g x = x : applyEveryOther' False f g (f x) 
        applyEveryOther' False f g x = x :  applyEveryOther' True f g (g x)

lookup :: Map Int Int -> (Int -> Int)
lookup m x = fromJust $ Map.lookup x m 

p :: State -> Int -> [Int]
p (f, g, _) = applyEveryOther (lookup f) (lookup g)

isOuter :: State -> Int -> Bool
isOuter (f, g, _) v = fv == v || lookup g fv /= fv
    where fv = lookup f v 

isInner :: State -> Int -> Bool
isInner (f, g, _) v = lookup g fv == fv && gv /= v
    where 
        fv  = lookup f v
        gv = lookup g v

isOutOfForest :: State -> Int -> Bool
isOutOfForest (f, g, _) v = fv /= v && gv == v && gfv == fv
    where
        fv  = lookup f v
        gv = lookup g v
        gfv = lookup g fv
        
scanned = undefined

edmonds graph =
    let nVertices   = (length . vertices) graph
        nEdges      = (length . edges) graph
        idMap       = Map.fromList [(x, x) | x <- [1..nVertices]]
        fInit       = idMap
        gInit     = idMap
        roInit      = idMap
        scannedInit = Map.fromList 
                        [(x, y) | x <- [1..nVertices], y <- replicate 10 False]
    in
        fInit

