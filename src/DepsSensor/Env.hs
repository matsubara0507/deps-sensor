module DepsSensor.Env where

import           RIO

import           Data.Extensible
import           DepsSensor.Config
import qualified Mix.Plugin.GitHub as MixGitHub

type Env = Record
  '[ "logger" >: LogFunc
   , "github" >: MixGitHub.Token
   , "config" >: Config
   ]
