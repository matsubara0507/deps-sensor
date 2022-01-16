{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module DepsSensor.Assets where

import           RIO

import           Data.FileEmbed

indexHtml :: ByteString
indexHtml = $(embedFile "assets/index.html")

indexJS :: ByteString
indexJS = $(embedFile "assets/index.js")

mainJS :: ByteString
#ifdef MAINJS_FILE
mainJS = $(embedFile MAINJS_FILE)
#else
mainJS = $(embedFile "assets/main.js")
#endif
