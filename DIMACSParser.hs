
module DIMACSParser (fileToGraph) where

import Data.Char
import Data.List
import Data.Graph

data DimacsEntry = DimacsSize 
    { nvertices :: Int
    , nedges    :: Int
    }
                 | DimacsEdge 
    { first     :: Int
    , second    :: Int
    }
                 | DimacsComment 
    { content   :: String
    } deriving (Show, Eq)

parseLine :: [String] -> DimacsEntry
parseLine ["p", "edge", x, y] = DimacsSize (read x) (read y)
parseLine ["e", x, y]         = DimacsEdge (read x) (read y)
parseLine ["a", x, y]         = DimacsEdge (read x) (read y)
parseLine ("c" : xs)          = DimacsComment $ unwords xs

loadLines :: [String] -> [DimacsEntry]
loadLines = map (parseLine . words)

loadFile :: FilePath -> IO [DimacsEntry]
loadFile path = fmap (loadLines . lines) (readFile path)

fileToGraph :: FilePath -> IO Graph
fileToGraph path    = do
    dimacs <- loadFile path
    let dimacsSize  = head [x | x@DimacsSize {} <- dimacs]
        dimacsEdges = [x | x@DimacsEdge {} <- dimacs]
        edges       = map (\(DimacsEdge x y) -> (x, y)) dimacsEdges
        bounds      = (1, nvertices dimacsSize)
    return $ buildG bounds edges

