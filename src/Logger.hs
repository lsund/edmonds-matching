
{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Protolude
import Data.Text as Text (append)

newtype Logger = Logger { content :: Text }

write :: Text -> Logger -> Logger
write msg logger = Logger $ content logger `append` msg `append` "\n"

writev :: Show a => Text -> a -> Logger -> Logger
writev msg val logger = 
    Logger $ content logger `append` msg `append` show val `append` "\n"

read = content
