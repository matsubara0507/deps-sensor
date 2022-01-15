{-# LANGUAGE CPP #-}

module Main where

import           RIO

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import qualified DepsSensor.Cmd         as Cmd
import           DepsSensor.Config
import           DepsSensor.Env         (Output (..))
import           GetOpt                 (withGetOpt')
import           Mix
import           Mix.Plugin.Config      as MixConfig
import qualified Mix.Plugin.GitHub      as MixGitHub
import           Mix.Plugin.Logger      as MixLogger
import           System.Environment     (getEnv)

main :: IO ()
main = withGetOpt' "[options] [config-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help              -> hPutBuilder stdout (fromString usage)
     | r ^. #version           -> hPutBuilder stdout (fromString VERSION_deps_sensor <> "\n")
     | otherwise               -> runCmd r (listToMaybe args)
  where
    opts = #help     @= helpOpt
        <: #version  @= versionOpt
        <: #verbose  @= verboseOpt
        <: #json     @= jsonOpt
        <: #generate @= generateOpt
        <: nil

type Options = Record
  '[ "help"     >: Bool
   , "version"  >: Bool
   , "verbose"  >: Bool
   , "json"     >: Bool
   , "generate" >: Maybe FilePath
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

jsonOpt :: OptDescr' Bool
jsonOpt = optFlag [] ["json"] "Show result as JSON format."

generateOpt :: OptDescr' (Maybe FilePath)
generateOpt = optLastArg [] ["generate"] "PATH" "Generate HTML/JavaScript files to PATH"

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts path = do
  gToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  config <- readConfig $ fromMaybe "./config.yaml" path
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #github <@=> MixGitHub.buildPlugin gToken
            <: #config <@=> MixConfig.buildPlugin config
            <: #output <@=> pure (if opts ^. #json then JSON else Simple)
            <: #work   <@=> pure "."
            <: nil
  case opts ^. #generate of
    Just p  -> Mix.run plugin $ Cmd.generateHtml p
    Nothing -> Mix.run plugin Cmd.displayDeps
  where
    logOpts = #handle @= stdout
           <: #verbose @= (opts ^. #verbose)
           <: nil
