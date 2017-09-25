{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Prelude ()
import Protolude
import Util

import Data.Graph
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

-- A typeclass for things that can be parsed by this parser
class Parseable a where
    parse :: [Text] -> a

-- A Data point represented by an entry in a DIMACS-file
data DimacsEntry =
       DimacsSize     { nvertices :: Int , nedges :: Int }
     | DimacsEdge     { first     :: Int , second :: Int }
     | DimacsComment  { content   :: Text }
     deriving (Show, Eq)

-- A Data point represented by an entry in a Optima-file
data OptimaEntry =
       Optima { path :: FilePath, optima :: Int }
     | Void deriving (Show)

-- Parsable implementation for DimacsEntry
instance Parseable DimacsEntry where
    parse ["p", "edge", x, y]  = DimacsSize (textToInt x) (textToInt y)
    parse [c, x, y]
        | c == "e" || c == "a" = DimacsEdge (textToInt x) (textToInt y)
    parse ("c" : xs)           = DimacsComment $ Text.unwords xs
    parse []                   = DimacsComment ""
    parse (_ : _)              = undefined

-- Parsable implementaion for Optima
instance Parseable OptimaEntry where
    parse [x, y]  = Optima (Text.unpack x) (textToInt y)
    parse (_ : _) = Void
    parse []      = Void

--------------------------------------------------------------------------------
-- IO 

-- Given a file path, return a list of lines, represented as lists of text
parseFile :: FilePath -> IO [[Text]]
parseFile path = fmap ((map Text.words) . Text.lines) (readFile path)

-- Given a file path, return a graph represented by the contents of this file
dimacsToGraph :: FilePath -> IO Graph
dimacsToGraph path    = do
    content <- parseFile path
    let dimacs = map parse content
        dimacsSize  = fromMaybe undefined $ head [x | x@DimacsSize {} <- dimacs]
        dimacsEdges = [x | x@DimacsEdge {} <- dimacs]
        edges       = map (\(DimacsEdge x y) -> (x, y)) dimacsEdges
        bounds      = (1, nvertices dimacsSize)
    return $ buildG bounds edges
