module Tree where

import qualified Data.Map as Map

type Tree = (Int, Map.Map Int [Int])

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
-- Construction

singleton :: Int -> Tree
singleton x = (x, Map.singleton x [])

insert :: Int -> Int -> Tree -> Tree
insert x at (root, tree) =
    case Map.lookup at tree of
        Just nbs -> (root, Map.adjust (x :) at tree)
        Nothing  -> (root, Map.insert at [x] tree)

----------------------------------------------------------------------------
-- Properties

-- All vertices with even distances to the root (The root itself is outer)
outer :: Tree -> [Int]
outer (root, tree) = root : outer' False tree root

outer' :: Bool -> Map.Map Int [Int] -> Int -> [Int]
outer' flag tree at =
    case Map.lookup at tree of
        Just nbs -> 
            let new = if flag then nbs else []
            in  new ++ concatMap (outer' (not flag) tree) nbs
        Nothing -> []

