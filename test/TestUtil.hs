module TestUtil where

import Protolude
import Test.HUnit

import Util

test00 = TestCase $ assertEqual "" [1] (takeWhileDifferent [1])

test01 = TestCase $ assertEqual "" [1,2] (takeWhileDifferent [1, 2])

test02 = TestCase $ assertEqual "" [1,2,3,4] (takeWhileDifferent [1,2,3,4])

test03 = TestCase $ assertEqual "" [1,2] (takeWhileDifferent [1,2,2,2])

test04 = TestCase $ assertEqual "" [1,2] (takeWhileDifferent [1,2,2,3,4])

utilTests = TestList [test00, test01, test02, test03, test04]
