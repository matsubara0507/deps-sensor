module DepsSensor.Deps where

import           RIO

import           Data.Extensible
import qualified Data.Yaml       as Y

type Deps = Record
  '[ "repository" >: Text -- expect owner/name
   , "snapshot"   >: Text -- stackage stapshot
   ]
