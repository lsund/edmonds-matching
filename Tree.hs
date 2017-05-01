module Tree where

import qualified Data.Map as Map
import Data.Array
import qualified Data.Set as Set

type Tree = Map.Map Int [Int]

----------------------------------------------------------------------------
-- dummy values

dummy :: Tree
dummy = insert 4 2 $
        insert 5 2 $
        insert 3 1 $
        insert 2 1 $
        insert 6 5 $
        singleton 1
----------------------------------------------------------------------------
-- Conversion


-- graphToTree :: Graph -> Tree
graphToTree graph = Map.fromList $ assocs graph


----------------------------------------------------------------------------
-- Construction

singleton :: Int -> Tree
singleton x = Map.singleton x []

insert :: Int -> Int -> Tree -> Tree
insert x at tree =
    case Map.lookup at tree of
        Just nbs -> Map.adjust (x :) at tree
        Nothing  -> Map.insert at [x] tree

----------------------------------------------------------------------------
-- Properties

rootValue :: Tree -> Int
rootValue tree = 
    let seconds = (Set.fromList . concat . Map.elems) tree
        firsts  = (Set.fromList . Map.keys) tree
    in Set.elemAt 0 (firsts `Set.difference` seconds)

-- All vertices with even distances to the root (The root itself is outer)
outer :: Tree -> [Int]
outer tree = root : outer' False tree root
    where root = rootValue tree

outer' :: Bool -> Map.Map Int [Int] -> Int -> [Int]
outer' flag tree at =
    case Map.lookup at tree of
        Just nbs -> 
            let new = if flag then nbs else []
            in  new ++ concatMap (outer' (not flag) tree) nbs
        Nothing -> []

