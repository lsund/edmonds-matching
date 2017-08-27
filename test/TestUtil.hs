module TestUtil where

import Protolude
import Test.HUnit
import Edmond.Data.Graph.Core

import Util

----------------------------------------------------------------------------
-- takeWhileDifferent
test00 = TestCase $ assertEqual "" [1] (takeWhileDifferent [1])

test01 = TestCase $ assertEqual "" [1,2] (takeWhileDifferent [1, 2])

test02 = TestCase $ assertEqual "" [1,2,3,4] (takeWhileDifferent [1,2,3,4])

test03 = TestCase $ assertEqual "" [1,2] (takeWhileDifferent [1,2,2,2])

test04 = TestCase $ assertEqual "" [1,2] (takeWhileDifferent [1,2,2,3,4])

----------------------------------------------------------------------------
-- uniqueElements

test05 = TestCase $ assertBool "" (uniqueElements [1,2,3])

test06 = TestCase $ assertBool "" (uniqueElements ([] :: [Int]))

test07 = TestCase $ assertBool "" (not (uniqueElements [1,2,2]))

test08 = TestCase $ assertBool "" (uniqueElements [3,2,1])

----------------------------------------------------------------------------
-- isMatching

test09 = TestCase $ assertBool "" (isMatching ([] :: [Edge]))

test10 = TestCase $ assertBool "" (isMatching [(1,2), (3, 4)])

test11 = TestCase $ assertBool "" (not (isMatching [(1,2), (2, 1)]))

utilTests = TestList [test00,
                      test01,
                      test02,
                      test03,
                      test04,
                      test05,
                      test06,
                      test07,
                      test08,
                      test09,
                      test10,
                      test11]
