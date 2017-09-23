module Util where

import Data.Graph
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import qualified Data.List as List
import Prelude ()
import Protolude

-- True if the second list contains at least one of the elements in the first
-- list
containsOne :: Eq a => [a] -> [a] -> Bool
containsOne xs ys = any (`elem` ys) xs

takeUntil :: Eq a => a -> [a] -> [a]
takeUntil _ [] = []
takeUntil v (x : xs)
  | v == x = [v]
takeUntil v (x : xs) = x : takeUntil v xs

takeWhileDifferent :: Eq a => [a] -> [a]
takeWhileDifferent [] = []
takeWhileDifferent [x] = [x]
takeWhileDifferent (x:y:xs)
  | x == y = [x]
takeWhileDifferent (x:y:xs) = x : takeWhileDifferent (y : xs)

iterateEveryOther :: (a -> a) -> (a -> a) -> a -> [a]
iterateEveryOther = iterateEveryOther' True
  where
    iterateEveryOther' True f g x = x : iterateEveryOther' False f g (f x)
    iterateEveryOther' False f g x = x : iterateEveryOther' True f g (g x)

areDisjoint :: Ord a => [a] -> [a] -> Bool
areDisjoint xs ys = null (xs `List.intersect` ys)

insertList :: [(Int, Int)] -> IntMap Int -> IntMap Int
insertList xs m = foldr (uncurry Map.insert) m xs

insertListSymmetric :: [(Int, Int)] -> IntMap Int -> IntMap Int
insertListSymmetric xs m =
  foldr (\(x, y) m -> Map.insert x y (Map.insert y x m)) m xs

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
symmetricUpdate :: (Int -> Int) -> Int -> Int -> Int -> IntMap Int -> IntMap Int
symmetricUpdate p t k v m =
  let xm =
        if p k /= t
          then Map.insert k v m
          else m
  in if p v /= t
       then Map.insert v k xm
       else xm

every :: Int -> [Int] -> IntSet -> IntSet
every n xs acc =
  case drop (pred n) xs of
    y:ys -> every n ys (Set.insert y acc)
    [] -> acc

-- In unspecified order
oddElements :: [a] -> [a]
oddElements = oddElements' []
  where
    oddElements' acc xs =
      case xs of
        y : ys -> oddElements'  (y : acc) (drop 2 xs)
        [] -> acc

-- In unspecified order
evenElements :: [a] -> [a]
evenElements = evenElements' []
  where
    evenElements' acc xs =
      case drop 1 xs of
        y : ys -> evenElements' (y : acc) ys 
        [] -> acc

appendIf :: Bool -> a -> [a] -> [a]
appendIf p x xs =
  if p
    then x : xs
    else xs

uniqueElements :: Ord a => [a] -> Bool
uniqueElements xs = length (List.nub xs) == length xs

isMatching :: [Edge] -> Bool
isMatching xs = uniqueElements vs
  where
    vs = map fst xs ++ map snd xs
