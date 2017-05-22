
module Util where

import Protolude
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text

iterateEveryOther :: (a -> a) -> (a -> a) -> a -> [a]
iterateEveryOther = iterateEveryOther' True
    where
        iterateEveryOther' True f g x  = x : iterateEveryOther' False f g (f x)
        iterateEveryOther' False f g x = x :  iterateEveryOther' True f g (g x)

areDisjoint :: Ord a => Set a -> Set a -> Bool
areDisjoint xs = Set.null . Set.intersection xs

adjustMap :: Ord a =>  a -> b -> Map a b -> Map a b
adjustMap k v = Map.adjust (const v) k

adjustMapFor :: Ord a =>  [a] -> [b] -> Map a b -> Map a b
adjustMapFor keys vals m = foldr (uncurry adjustMap) m (zip keys vals)

every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
                (y:ys) -> y : every n ys
                [] -> []

appendIf :: Bool -> a -> [a] -> [a]
appendIf p x xs = if p then x : xs else xs

