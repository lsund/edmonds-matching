{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Tests where

import Protolude
import Test.HUnit

import TestParser (dimacsTests)
import TestAlgorithm (mAlgoTests)
import TestUtil (utilTests)

import Data.List

----------------------------------------------------------------------------
-- utilities

sameElements xs ys = null $ xs \\ ys

----------------------------------------------------------------------------
-- All Tests

runtests :: IO Counts
runtests = do
    algoTests <- mAlgoTests
    runTestTT $ TestList [utilTests, dimacsTests, algoTests]

