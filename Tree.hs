module Tree where

import Data.Tree

----------------------------------------------------------------------------
-- dummy values

dummy = insert 5 2 $ insert 4 2 $ insert 2 1 $ insert 3 1 $ singleton 1

----------------------------------------------------------------------------
-- Construction

singleton :: Int -> Tree Int
singleton x = Node x []

insert :: Int -> Int -> Tree Int -> Tree Int
insert x under (Node v cs)
    | under == v = Node v (Node x [] : cs)
    | otherwise  = Node v (map (insert x under) cs)

----------------------------------------------------------------------------
-- Properties

-- All vertices with even distances to the root (The root itself is outer)
outer :: Tree Int -> [Int]
outer = outer' True
    where
        outer' True (Node v cs) = v : concatMap (outer' False) cs
        outer' False (Node v cs) = concatMap (outer' True) cs

----------------------------------------------------------------------------
-- Utilities

toScreen :: Tree Int -> IO ()
toScreen = putStrLn . drawTree . fmap show
