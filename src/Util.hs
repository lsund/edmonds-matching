{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Util where

import Prelude ()
import Protolude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Text (append)
import Data.Graph
import Data.Hashable
import qualified Data.HashTable.ST.Basic as HashTable

type HashMap = HashMap.HashMap
type HashTable = HashTable.HashTable

----------------------------------------------------------------------------
-- Debug

(+++) :: Text -> Text -> Text
a +++ b = a `append` b

-- True if the second list contains at least one of the elements in the first
-- list
containsOne :: Eq a => [a] -> [a] -> Bool
containsOne xs ys = any (`elem` ys) xs

takeUntil :: Ord a => a -> [a] -> (Set a, Set a)
takeUntil v xs = 
    let 
        takeUntil' v [] _ evens odds               = (evens, odds)
        takeUntil' v (x : xs) True evens odds
            | v == x = (evens, Set.insert x odds)
        takeUntil' v (x : xs) False evens odds
            | v == x = (Set.insert x evens, odds)
        takeUntil' v (x : xs) True evens odds  =
            takeUntil' v xs False evens (Set.insert x odds)
        takeUntil' v (x : xs) False evens odds = 
            takeUntil' v xs True (Set.insert x evens) odds
        in
            takeUntil' v xs False Set.empty Set.empty

takeWhileDifferent :: Ord a => [a] -> (Set a, Set a)
takeWhileDifferent xs =
    let 
        takeWhileDifferent' [] isodd evens odds = (evens, odds)
        takeWhileDifferent' (x : y : xs) isodd evens odds | x == y =
            takeWhileDifferent' [x] isodd evens odds
        takeWhileDifferent' [x] True evens odds  =
            (evens, Set.insert x odds)
        takeWhileDifferent' [x] False evens odds =
            (Set.insert x evens, odds)
        takeWhileDifferent' (x : y : xs) True evens odds =
            takeWhileDifferent' (y : xs) False evens (Set.insert x odds)
        takeWhileDifferent' (x : y : xs) False evens odds =
            takeWhileDifferent' (y : xs) True (Set.insert x evens) odds
    in
         takeWhileDifferent' xs False Set.empty Set.empty

iterateEveryOther :: (a -> ST s a) -> (a -> ST s a) -> a -> ST s [a]
iterateEveryOther = iterateEveryOther' True
    where
        iterateEveryOther' :: Bool -> (a -> ST s a) -> (a -> ST s a) -> a -> ST s [a]
        iterateEveryOther' True f g x = do
            let fx = f x
            x' <- fx
            xs <- iterateEveryOther' False f g x'
            return $ x : xs
        iterateEveryOther' False f g x = do
            let gx = g x
            x' <- gx
            xs <- iterateEveryOther' False f g x'
            return $ x :  xs

areDisjoint :: Ord a => [a] -> [a] -> Bool
areDisjoint xs ys = null (xs `List.intersect` ys)

adjustMap :: (Eq a, Hashable a) =>  a -> b -> HashMap a b -> HashMap a b
adjustMap k v = HashMap.adjust (const v) k

adjustMapFor :: (Eq a, Hashable a) =>  [(a, a)] -> HashMap a a -> HashMap a a
adjustMapFor xs m = foldr (uncurry adjustMap) m xs

adjustMapForSymmetric :: (Eq a, Hashable a) => [(a, a)] -> HashMap a a -> HashMap a a
adjustMapForSymmetric xs m = foldr (\(x, y) m -> adjustMap x y (adjustMap y x m)) m xs 


adjustHashTable :: (Eq a, Hashable a) =>
                   (a, b) ->
                    HashTable s a b ->
                    ST s ()
adjustHashTable (k, v) ht = HashTable.insert ht k v


adjustHashTableFor :: (Eq a, Hashable a) =>
                      [(a, a)] ->
                      HashTable s a a ->
                      ST s ()
adjustHashTableFor xs ht = mapM_ (uncurry (HashTable.insert ht)) xs
     
adjustHashTableForSymmetric :: (Eq a, Hashable a, Foldable t) =>
                               t (a, a) ->
                               HashTable s a a ->
                               ST s ()
adjustHashTableForSymmetric xs ht =
    mapM_ (\(k, v) -> do
            HashTable.insert ht k v
            HashTable.insert ht v k)
          xs

--  Arguments:
--
--  p: the predicate, 
--  t: the target
--  k: the key
--  v: the value
--  m: the map
--
--  Description:
--
--  Updates the map with (k, v) if the key applied to the predicate doesn't
--  match the target.
--  Updates the map with (v, k) if the value applied to the predicate doesn't
--  match the target.
--  
symmetricUpdate :: (Eq a, Hashable a) => (a -> a) -> a -> a -> a -> HashMap a a -> HashMap a a
symmetricUpdate p t k v m = 
    let xm = if p k /= t then adjustMap k v m else m
    in       if p v /= t then adjustMap v k xm else xm

every :: Int -> [a] -> [a]
every n xs = 
    case drop (pred n) xs of
        y : ys -> y : every n ys
        []    -> []

odds :: [a] -> [a] -> ([a], [a])
odds px py = (every 2 px, every 2 py)

appendIf :: Bool -> a -> [a] -> [a]
appendIf p x xs = if p then x : xs else xs

uniqueElements :: Ord a => [a] -> Bool
uniqueElements xs = length (List.nub xs) == length xs

isMatching :: [Edge] -> Bool
isMatching xs = uniqueElements vs
    where vs = map fst xs ++ map snd xs

