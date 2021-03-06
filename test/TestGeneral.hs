{-# LANGUAGE OverloadedStrings #-}

module TestGeneral where

import Protolude
import Test.HUnit
import Util
import qualified Data.Text as Text

import Data.Graph.Core as Graph
import Parser
import Algorithm.Edmonds.Core
import Algorithm.Edmonds.General.Core
import Algorithm.Heuristics.Core

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
    let m = toMatching graph
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
        let matching = findMatching None rep
        checkMatchingLen len matching)

testMatchingLenHeuristic :: (FilePath, Int) -> Test
testMatchingLenHeuristic (path, len) =
    TestCase (do
        rep <- fileToGraph path
        let matching = findMatching GreedyMaximal rep
        checkMatchingLen len matching)

----------------------------------------------------------------------------
-- tests

tests1 :: IO [Test]
tests1 = do content <- parseFile "data/optima-stripped.txt"
            let optimas = [x | x@Optima {} <- map parse content]
                names   = map Parser.path optimas
                paths = map ("data/graphs/" ++) names
                optima  = map Parser.optima optimas
            return $
                    zipWith (curry testMatchingLen) paths optima
                ++  zipWith (curry testMatchingLenHeuristic) paths optima
                ++  map testIfMatching paths


mGeneralTests :: IO Test
mGeneralTests = do
    t1 <- tests1
    return $ TestList t1

