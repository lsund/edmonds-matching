{-# LANGUAGE OverloadedStrings #-}

module Generator where

import Prelude ()
import Protolude

import Data.Text as Text (append)
import Data.Graph
import Data.List (nub)
import Util

import System.Random
import System.Directory (createDirectory)

-- DIMACS format representation of an edge
dimacsRep :: (Int, Int) -> Text
dimacsRep (x, y) =
    let line = ["e ", show x, " ", show y, "\n"]
    in foldr Text.append "" line

--------------------------------------------------------------------------------
--  IO

-- returns a list of random edges between vertices `[1..nv]`
-- of a maximum length specified by `ne`.
randomEdges :: Int -> Int -> IO [Edge]
randomEdges nv ne = do
    xs <- replicateM ne $ randomRIO (1, nv)
    ys <- replicateM ne $ randomRIO (1, nv)
    return $ uniqueFlipped $ zip xs ys

-- Given a list of edges and a filepath, write a DIMACS-formatted graph
-- represented by the list of edges.
writeGraph :: FilePath -> [Edge] -> IO ()
writeGraph p es = do
    let nv = maxVertex es
    let firstline = [show nv, " ", show (length es), "\n"]
    writeFile p ""
    appendFile p (foldl Text.append "p edge " firstline)
    mapM_ (appendFile p . dimacsRep) es

-- Given a number of maximum vertices and edges and a filepath, write a random
-- DIMACS-formatted graph .
writeRandomGraph :: Int -> Int -> FilePath -> IO ()
writeRandomGraph nv ne p = randomEdges nv ne >>= writeGraph p


-- Given a number, return a list of filenames of the form [001.dmx, 002.dmx ...]
genFilenames :: Int -> [FilePath]
genFilenames n = genFilenames' n n
    where
        genFilenames' _ count | count == 0 = []
        genFilenames' _ count | count > 999 = undefined
        genFilenames' n' count = name count : genFilenames' n' (pred count)
            where 
                name c | c < 10 = "00" ++ show count ++ ".dmx"
                name c | c < 100 = "0" ++ show count ++ ".dmx"
                name _           = show count ++ ".dmx"

-- Given a number of vertices, edges and files and a path, generate a
-- corresponding number of files containing random DIMACS-graphs with a maximum
-- number of vertices and edges.
genRandomDir :: Int -> Int -> Int -> FilePath -> IO ()
genRandomDir nv ne nfiles p = do
    createDirectory p
    mapM_ 
        (\ fname -> writeRandomGraph nv ne (p ++ "/" ++ fname)) 
        (genFilenames nfiles)
