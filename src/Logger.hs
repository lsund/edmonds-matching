
{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Protolude
import Data.Text as Text (append)

newtype Logger = Logger { content :: Text }

write :: Logger -> Text -> Logger
write logger msg = Logger $ content logger `append` msg `append` "\n"

read = content
