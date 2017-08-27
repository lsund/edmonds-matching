module Main where

import Protolude
import Test.HUnit

import TestParser (dimacsTests)
import TestGeneral (mGeneralTests)
import TestUtil (utilTests)
import TestGraph (mGraphTests)
import TestHeuristics (mHeuristicTests)

import Data.List

----------------------------------------------------------------------------
-- utilities

sameElements xs ys = null $ xs \\ ys

----------------------------------------------------------------------------
-- All Tests

runtests :: IO Counts
runtests = do
    algoTests <- mGeneralTests
    graphTests <- mGraphTests
    heuristicTests <- mHeuristicTests
    runTestTT $ TestList [utilTests, dimacsTests, graphTests, algoTests, heuristicTests]

main :: IO ()
main = runtests >>= print
