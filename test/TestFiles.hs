{-# LANGUAGE OverloadedStrings #-}
module TestFiles where

import Protolude
import qualified Data.Text as Text
import qualified Data.List.Split as LS
import qualified Data.List as L
import System.Directory

import Edmond.Algorithm.Core
import Parser

testFile :: FilePath ->  FilePath -> IO ()
testFile inpath outpath = do
    graph <- fileToGraph inpath
    es <- edmonds graph
    let basename = L.last $ LS.splitOn "/" inpath
    appendFile outpath (Text.pack $ basename ++ " " ++ show (length es) ++ "\n")

testDirectory :: FilePath -> FilePath -> IO ()
testDirectory inpath outpath = do
    writeFile outpath ""
    mpaths >>= mapM_ (\x -> testFile (inpath ++ x) outpath) . sort
    where mpaths = System.Directory.listDirectory inpath
