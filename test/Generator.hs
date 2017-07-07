{-# LANGUAGE OverloadedStrings #-}
module Generator where
import Data.Text as Text (append)
import Protolude
import Data.Graph
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

genRandomEdges :: Int -> IO [Edge]
genRandomEdges nv = do
    let neMax = div (nv * (nv - 1)) 2
        ne = ceiling $ fromIntegral neMax * sparseness
    randomEdges nv ne

format (x, y) = 
    let line = ["e ", show x,  " ", show y, "\n"]
    in foldr Text.append "" line

maxVertex :: [Edge] -> Vertex
maxVertex xs = max x y
    where ((x, y) : _) = 
            sortBy (\(a, b) (c, d) -> if max a b > max c d then LT else GT) xs

-- writeGraph :: [Edge] -> FilePath -> IO ()
-- writeGraph edges path = do
--     let nv = maxBound edges
--     let firstline = [show nv, " ", show (length edges), "\n"]
--     appendFile path (foldl Text.append "p edge " firstline)
--     mapM_ (appendFile path . format) edges
    
-- writeRandomGraph :: Int -> FilePath -> IO ()
-- writeRandomGraph nv path = do
--     randomEdges <- genRandomEdges nv
--     writeGraph randomEdges


