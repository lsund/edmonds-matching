{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Edmond.Data.Assoc where

import Protolude
import qualified Data.Map.Strict as Map
import Data.Text (append)

----------------------------------------------------------------------------
-- Assoc
--
-- An association between two enteties. It exists to couple the two
-- associations: a dictionary and a function. The dictionary is a map from the
-- first to the second entity. The function is the map applied as a function
-- from the first to the second entity.
data Assoc a b = Assoc { dict :: Map a b
                       , fun :: a -> b }

-- Given a dictionary, creates a function
assocToFun :: (Show a, Ord a) => Map a b -> (a -> b)
assocToFun m x = Map.findWithDefault e x m 
    where e = error $ "cant find key: " `append` show x

-- Given a map, creates an association
makeAssoc :: (Show a, Ord a) => Map a b -> Assoc a b
makeAssoc m = Assoc m (assocToFun m)

