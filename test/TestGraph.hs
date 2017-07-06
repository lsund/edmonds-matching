{-# LANGUAGE OverloadedStrings #-}

module TestGraph where

import qualified Edmond.Data.Graph as Graph
import Test.HUnit
import Protolude
import Parser
import Generator

----------------------------------------------------------------------------
-- tests

mGraphTests :: IO Test
mGraphTests = do 
    rep <- fileToGraph "data/graphs/fixed.dmx"
    let graph = Graph.initialize rep
        nv = length $ Graph.vertices graph
        ne = length $ Graph.edges graph
        tests = [ TestCase $ assertEqual "Should have 1000 vertices" 1000 nv
                , TestCase $ assertEqual "Should have 1000 vertices" 449449 ne
                ]
    return $ TestList tests

