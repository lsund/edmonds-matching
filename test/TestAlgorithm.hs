{-# LANGUAGE OverloadedStrings #-}

module TestAlgorithm where

import Protolude
import Test.HUnit
import Util
import qualified Data.Text as Text

import Edmond.Data.Graph as Graph
import Parser
import Edmond.Algorithm.Core

----------------------------------------------------------------------------
-- Data

dataDir = "data/graphs/"

expectedLengths = [ 2
                  , 1
                  , 2
                  , 2
                  , 4
                  , 1
                  , 2
                  , 1
                  , 2
                  , 2
                  , 1
                  , 2
                  , 3
                  , 5
                  ]

----------------------------------------------------------------------------
-- Utils

checkIfMatching :: Graph -> Assertion
checkIfMatching graph =
    let m = matching graph
    in assertBool "Should be a matching" $ isMatching m && containsEdges m graph

checkMatchingLen :: Int -> [Edge] -> Assertion 
checkMatchingLen exp matching =
    assertEqual "Should have correct length " exp $ length matching

testIfMatching :: FilePath -> Test
testIfMatching path = TestCase  (do rep <- fileToGraph path
                                    let init = Graph.initialize rep
                                    checkIfMatching (findRoot init))

testMatchingLen :: (FilePath, Int) -> Test
testMatchingLen (path, len) = 
    TestCase (do
        rep <- fileToGraph path
        matching <- edmonds rep
        checkMatchingLen len matching)

----------------------------------------------------------------------------
-- tests

tests1 :: IO [Test]
tests1 = do content <- parseFile "data/optima-stripped.txt"
            let optimas = [x | x@Optima {} <- map parse content]
                names   = map Parser.path optimas
                paths = map ("data/graphs/" ++) names
                optima  = map Parser.optima optimas
            mapM_ print paths
            return $ 
                zipWith (curry testMatchingLen) paths optima
                ++ map testIfMatching paths
                

mAlgoTests :: IO Test
mAlgoTests = do
    t1 <- tests1
    return $ TestList t1

