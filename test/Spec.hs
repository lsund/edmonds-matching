module Main where

import Protolude
import Test.HUnit

import TestParser (dimacsTests)
import TestGeneral (mGeneralTests)
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
    algoTests <- mGeneralTests
    graphTests <- mGraphTests
    runTestTT $ TestList [utilTests, dimacsTests, graphTests, algoTests]

main :: IO ()
main = runtests >>= print
