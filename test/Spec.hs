module Main where

import Protolude
import Test.HUnit

import TestParser (dimacsTests)
import TestAlgorithm (mAlgoTests)
import TestUtil (utilTests)
import TestGraph (mGraphTests)

import Data.List

----------------------------------------------------------------------------
-- utilities

sameElements xs ys = null $ xs \\ ys

----------------------------------------------------------------------------
-- All Tests

runtests :: IO Counts
runtests = do
    algoTests <- mAlgoTests
    graphTests <- mGraphTests
    runTestTT $ TestList [utilTests, dimacsTests, graphTests, algoTests]

main :: IO ()
main = runtests >>= print
