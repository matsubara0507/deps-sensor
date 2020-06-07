{-# LANGUAGE TemplateHaskell #-}

module DepsSensor.Assets where

import           RIO

import           Data.FileEmbed

indexHtml :: ByteString
indexHtml = $(embedFile "assets/index.html")

indexJS :: ByteString
indexJS = $(embedFile "assets/index.js")

mainJS :: ByteString
mainJS = $(embedFile "assets/main.js")
