{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Assoc where

import Protolude
import qualified Data.Map as Map
import Data.Maybe

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
assocToFun :: Ord a => Map a b -> (a -> b)
assocToFun m x = fromJust $ Map.lookup x m 

-- Given a map, creates an association
makeAssoc :: Ord a => Map a b -> Assoc a b
makeAssoc m = Assoc m (assocToFun m)

