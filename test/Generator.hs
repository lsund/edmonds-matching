module Generator where

import Protolude
import qualified Data.Graph
import System.Random
import Data.List (nub) 

sparseness = 0.3 :: Float

shapeTupleList :: [(Int, Int)] -> [(Int, Int)]
shapeTupleList ts = Data.List.nub $ filter (uncurry (/=)) sorted
    where sorted = map (\(x, y) -> if x < y then (x, y) else (y, x)) ts

-- returns a list of random edges of maximum length max
randomEdges nv ne = do
    xs <- replicateM ne $ randomRIO (1, nv)
    ys <- replicateM ne $ randomRIO (1, nv)
    return $ shapeTupleList $ zip xs ys

genGraph nv = do
    let neMax = div (nv * (nv - 1)) 2
        ne = ceiling $ fromIntegral neMax * sparseness
    edges <- randomEdges nv ne
    return $ Data.Graph.buildG (1, nv) edges
