{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Tests where

import Protolude
import Test.HUnit

import TestDIMACSParser (dimacsTests)
import TestAlgorithm (algoTests)
import TestUtil (utilTests)

import Data.List

----------------------------------------------------------------------------
-- utilities

sameElements xs ys = null $ xs \\ ys

----------------------------------------------------------------------------
-- All Tests

-- alltests :: [Test]
alltests = TestList [utilTests, dimacsTests, algoTests]

