module DepsSensor.Deps where

import           RIO

import           Data.Extensible

type Deps = Record
  '[ "repository" >: Text -- expect owner/name
   , "snapshot"   >: Text -- stackage stapshot
   ]

simpleDisplay :: Deps -> Text
simpleDisplay deps = deps ^. #repository <> ": " <> deps ^. #snapshot
