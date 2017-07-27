{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Util where

import Prelude ()
import Protolude
import qualified Data.List as List
import Data.Graph
import Data.Hashable
import qualified Data.HashTable.ST.Basic as HashTable

type HashTable = HashTable.HashTable

----------------------------------------------------------------------------
-- Debug

adjustHashTable :: (Eq a, Hashable a) =>
                   (a, b) ->
                    HashTable s a b ->
                    ST s ()
adjustHashTable (k, v) ht = HashTable.insert ht k v


adjustHashTableFor :: (Eq a, Hashable a) =>
                      [(a, b)] ->
                      HashTable s a b ->
                      ST s ()
adjustHashTableFor xs ht = mapM_ (uncurry (HashTable.insert ht)) xs
     
adjustHashTableForSymmetric :: (Eq a, Hashable a, Foldable t) =>
                               t (a, a) ->
                               HashTable s a a ->
                               ST s ()
adjustHashTableForSymmetric xs ht =
    mapM_ (\(k, v) -> do
            HashTable.insert ht k v
            HashTable.insert ht v k) xs

uniqueElements :: Ord a => [a] -> Bool
uniqueElements xs = length (List.nub xs) == length xs

isMatching :: [Edge] -> Bool
isMatching xs = uniqueElements vs
    where vs = map fst xs ++ map snd xs

