module TestMain where

import Protolude
import Data.List.Split
import Data.List (last)
import System.Directory

import Edmond.Algorithm.Core
import Parser

testFile :: FilePath -> IO ()
testFile path = do
    graph <- fileToGraph path
    es <- edmonds graph
    let basename = Data.List.last $ Data.List.Split.splitOn "/" path
    putStrLn $ basename ++ " " ++ show (length es)

testDirectory :: FilePath -> IO ()
testDirectory path = 
    mpaths >>= mapM_ (\x -> testFile (path ++ x)) . sort
    where mpaths = System.Directory.listDirectory path
