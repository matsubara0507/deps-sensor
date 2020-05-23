module DepsSensor.Config where

import           RIO

import           Data.Extensible
import qualified Data.Yaml       as Y

type Config = Record
  '[ "repositories" >: [Text] -- expect owner/name
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow
