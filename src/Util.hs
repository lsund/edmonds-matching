
module Util where

import Protolude
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Tuple
import Data.Text (append)
import Data.Graph
import qualified Data.Sequence as Seq

----------------------------------------------------------------------------
-- Debug

(+++) :: Text -> Text -> Text
a +++ b = a `append` b

debug :: Text -> a -> a
debug = trace

debugv :: Show a => Text -> a -> b -> b
debugv msg val = trace (msg +++ show val) 

debugvId :: Show a => Text -> a -> a
debugvId msg expr = trace (msg +++ show expr) expr

-- True if the second list contains at least one of the elements in the first
-- list
containsOne :: Eq a => [a] -> [a] -> Bool
containsOne xs ys = any (`elem` ys) xs

takeUntil :: Eq a => a -> [a] -> [a]
takeUntil _ []                = []
takeUntil v (x : xs) | v == x = [v]
takeUntil v (x : xs)          = x : takeUntil v xs

takeWhileDifferent :: Eq a => [a] -> [a]
takeWhileDifferent []                    = []
takeWhileDifferent [x]                   = [x]
takeWhileDifferent (x : y : xs) | x == y = [x]
takeWhileDifferent (x : y : xs)          = x : takeWhileDifferent (y : xs)

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

adjustMapFor2 :: Ord a =>  Seq a -> Seq b -> Map a b -> Map a b
adjustMapFor2 keys vals m = foldr (uncurry adjustMap) m (Seq.zip keys vals)

every :: Int -> Seq a -> Seq a
every n xs = 
    case Seq.viewl (Seq.drop (pred n) xs) of
        Seq.EmptyL    -> Seq.empty
        (y Seq.:< ys) -> every n ys

appendIf :: Bool -> a -> [a] -> [a]
appendIf p x xs = if p then x : xs else xs

uniqueElements :: Ord a => [a] -> Bool
uniqueElements xs = length (List.nub xs) == length xs

isMatching :: [Edge] -> Bool
isMatching xs = uniqueElements vs
    where vs = map fst xs ++ map snd xs

