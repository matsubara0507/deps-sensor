{-# LANGUAGE TemplateHaskell #-}

module DepsSensor.Assets where

import           RIO

import           Data.FileEmbed

indexHtml :: ByteString
indexHtml = $(embedFile "assets/index.html")
