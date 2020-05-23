module Main where

import           Paths_deps_sensor      (version)
import           RIO

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           DepsSensor.Cmd
import           DepsSensor.Config
import           GetOpt                 (withGetOpt')
import           Mix
import           Mix.Plugin.Config      as MixConfig
import qualified Mix.Plugin.GitHub      as MixGitHub
import           Mix.Plugin.Logger      as MixLogger
import           System.Environment     (getEnv)
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [config-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version <> "\n")
     | otherwise     -> runCmd r (listToMaybe args)
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts path = do
  gToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  config <- readConfig $ fromMaybe "./config.yaml" path
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #github <@=> MixGitHub.buildPlugin gToken
            <: #config <@=> MixConfig.buildPlugin config
            <: nil
  Mix.run plugin cmd
  where
    logOpts = #handle @= stdout
           <: #verbose @= (opts ^. #verbose)
           <: nil
