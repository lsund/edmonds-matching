module Util where

import Prelude ()
import Protolude

import Data.Text.Read as Text (decimal)
import Data.Graph
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import qualified Data.List as List

-- True if the list of edges contain no duplicate vertices
isMatching :: [Edge] -> Bool
isMatching xs = uniqueElements vs
  where
    vs = map fst xs ++ map snd xs
    
-- Try to parse a string of text into an integer
textToInt :: Text -> Int
textToInt s = either undefined fst (Text.decimal s)

-- Given a list of Edges, return the unique list of Edges [(x, y)]
-- where each x < y
uniqueFlipped :: [(Int, Int)] -> [(Int, Int)]
uniqueFlipped ts = List.nub $ filter (uncurry (/=)) sorted
    where sorted = map (\ (x, y) -> if x < y then (x, y) else (y, x)) ts

-- The maximum vertex contained in a list of vertices
maxVertex :: [Edge] -> Vertex
maxVertex xs = max x y
    where
      ((x, y) : _) =
        sortBy (\ (a, b) (c, d) -> if max a b > max c d then LT else GT) xs

-- A map over a 3-tuple
tuple3map :: (a -> b) -> (a, a, a) -> (b, b, b)
tuple3map    f           (a, b, c) =  (f a, f b, f c)

-- True if one floating point number in the tuple is considered zero
tuple3AnyZero :: (Floating a, Ord a) => (a, a, a) -> Bool
tuple3AnyZero (x, y, z) = any (\x' -> x' < small) [x, y, z]
  where small = 1.0e-2

-- Round a number two a specified number of decimals
roundN :: (Integral b, Fractional a, RealFrac t) => b -> t -> a
roundN n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n) 

-- The norm of a 3-tuple seen as a vector
norm3 :: Floating a => (a, a, a) -> a
norm3 (x, y, z) = sqrt (x ** 2 + y ** 2 + z ** 2)

-- The normalized 3-tuple seen as a vector
normalize3 :: Floating a  => (a, a, a) -> (a, a, a)
normalize3 v = tuple3map (/ nrm) v
  where
    nrm = norm3 v

-- The average of a list of 3-tuples
average3 :: (Floating a, RealFrac a) => [(a, a, a)] -> (a, a, a)
average3 xs =
  let len    = fromIntegral $ length xs
      sum3      = \(ta, tb, tc) (a, b, c) -> (ta + a, tb + b, tc + c)
      summed = foldl sum3 (0.0, 0.0, 0.0) xs
      avg    = tuple3map ((roundN 3) . (/ len)) summed
  in avg

-- True if the second list contains at least one of the elements in the first
-- list
containsOne :: Eq a => [a] -> [a] -> Bool
containsOne xs ys = any (`elem` ys) xs

-- Like takewhile with `(/= v)` but includes v
takeUntil :: Eq a => a -> [a] -> [a]
takeUntil _ []                = []
takeUntil v (x : xs) | v == x = [v]
takeUntil v (x : xs)          = x : takeUntil v xs

-- Takes elements in a list until two successive elements are different
takeWhileDifferent :: Eq a => [a] -> [a]
takeWhileDifferent []                   = []
takeWhileDifferent [x]                  = [x]
takeWhileDifferent (x : y: xs) | x == y = [x]
takeWhileDifferent (x:y:xs)             = x : takeWhileDifferent (y : xs)

-- Returns the list [f x, g f x, f g f x ...]
iterate2 :: (a -> a) -> (a -> a) -> a -> [a]
iterate2 = iterate2' True
  where
    iterate2' True f g x = x : iterate2' False f g (f x)
    iterate2' False f g x = x : iterate2' True f g (g x)

-- Returns True if the two lists have a non-empty intersection
areDisjoint :: Ord a => [a] -> [a] -> Bool
areDisjoint xs ys = null (xs `List.intersect` ys)

-- Given a list of key-value pairs, insert each into a given map
insertList :: [(Int, Int)] -> IntMap Int -> IntMap Int
insertList xs m = foldr (uncurry Map.insert) m xs

-- Given a list of key-value pairs, insert each into a given map. Then
-- insert each pair again, but this time flipped. (k, v) ~> (v, k)
insertListSymmetric :: [(Int, Int)] -> IntMap Int -> IntMap Int
insertListSymmetric xs m =
  foldr (\(x, y) m -> Map.insert x y (Map.insert y x m)) m xs

--  Updates the map with (k, v) if the key applied to the predicate doesn't
--  match the target.
--  Updates the map with (v, k) if the value applied to the predicate doesn't
--  match the target.
symmetricUpdate :: (Int -> Int) -> Int -> Int -> Int -> IntMap Int -> IntMap Int
symmetricUpdate p t k v m =
  let xm =
        if p k /= t
          then Map.insert k v m
          else m
  in if p v /= t
       then Map.insert v k xm
       else xm

-- Every n'th element of the list in uspecified order
every :: Int -> [Int] -> IntSet -> IntSet
every n xs acc =
  case drop (pred n) xs of
    y:ys -> every n ys (Set.insert y acc)
    [] -> acc

-- Every second element of the list in unspecified order, starting with the first
oddElements :: [a] -> [a]
oddElements = oddElements' []
  where
    oddElements' acc xs =
      case xs of
        y : ys -> oddElements'  (y : acc) (drop 2 xs)
        [] -> acc

-- Every second element of the list in unspecified order, starting with the second
evenElements :: [a] -> [a]
evenElements = evenElements' []
  where
    evenElements' acc xs =
      case drop 1 xs of
        y : ys -> evenElements' (y : acc) ys 
        [] -> acc

-- True if the list contains only unique elements
uniqueElements :: Ord a => [a] -> Bool
uniqueElements xs = length (List.nub xs) == length xs

