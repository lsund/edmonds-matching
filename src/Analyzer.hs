{-# LANGUAGE OverloadedStrings #-}

module Analyzer where

import Prelude ()
import Protolude
import Util

import qualified Data.Text as Text
import Data.Text.Read (double)
import Data.List (zip3)
import System.Directory

-- Given a string of the form:
-- `( command )  TOTAL_TIME user USER_TIME system PERCENT cpu CPU_TIME total`
-- Extract the TOTAL_TIME as a double
extractTime :: Text -> Double
extractTime s = 
  let (_ : x : _) = Text.splitOn ")" s
      (t : _)     = Text.words x 
  in either undefined (\(a, _) -> a) $ double t

--------------------------------------------------------------------------------
-- IO 

-- Given a directory and a 3-tuple, write the 3-tuple to the file `res`
writeResult :: FilePath -> (Double, Double, Double) -> IO ()
writeResult path res = do
  putStrLn $ "echo " ++ (show res) ++ " > " ++ path ++ "/res"
  writeFile (path ++ "/res") (show res)

-- Given a directory containing the files no, gm and ec, with time entries as
-- described under `extractTime`, produce an average 3-tuple of the times and
-- write it to the file `res`
analyze :: FilePath -> IO ()
analyze path = do
  let (nopath, gmpath, ecpath) = (path ++ "/no", path ++ "/gm", path ++ "/ec")
  nocontent <- readFile nopath
  gmcontent <- readFile gmpath
  eccontent <- readFile ecpath
  let content = (nocontent, gmcontent, eccontent)
      ls = tuple3map Text.lines content
      (notimes, gmtimes, ectimes) = tuple3map (map extractTime) ls
      timeVectors = filter (not . tuple3AnyZero) $ zip3 notimes gmtimes ectimes
      normalized  = average3 $ map normalize3 timeVectors
  writeResult path normalized

analyzeAll :: FilePath -> IO ()
analyzeAll p = do
  dirs <- listDirectory p
  mapM_ (\x -> analyze (p ++ x)) dirs
