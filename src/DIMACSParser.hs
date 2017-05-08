{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module DIMACSParser (fileToGraph) where

import Protolude
import Data.Graph
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

data DimacsEntry = DimacsSize     { nvertices :: Int
                                  , nedges    :: Int }
                 | DimacsEdge     { first     :: Int
                                  , second    :: Int }
                 | DimacsComment  { content   :: Text } deriving (Show, Eq)

textToInt :: Text -> Int
textToInt x = case Text.decimal x of
                    Left a -> undefined
                    Right b -> fst b

parseLine :: [Text] -> DimacsEntry
parseLine ["p", "edge", x, y] = DimacsSize (textToInt x) (textToInt y)
parseLine [c, x, y]
    | c == "e" || c == "a"    = DimacsEdge (textToInt x) (textToInt y)
parseLine ("c" : xs)          = DimacsComment $ Text.unwords xs

loadLines :: [Text] -> [DimacsEntry]
loadLines = map (parseLine . Text.words)

loadFile :: FilePath -> IO [DimacsEntry]
loadFile path = fmap (loadLines . Text.lines) (readFile path)

fileToGraph :: FilePath -> IO Graph
fileToGraph path    = do
    dimacs <- loadFile path
    let dimacsSize  = fromMaybe undefined $ head [x | x@DimacsSize {} <- dimacs]
        dimacsEdges = [x | x@DimacsEdge {} <- dimacs]
        edges       = map (\(DimacsEdge x y) -> (x, y)) dimacsEdges
        bounds      = (1, nvertices dimacsSize)
    return $ buildG bounds edges

