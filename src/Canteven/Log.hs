{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{- |
  This module provides a way (not the best way, just *a* way) to easily set up
  logging for your program without all the hassle. It uses a standard
  configuration (obtained using the @canteven-config@ package) to set up the
  @hslogger@ configuration. Maybe in the future we will use some kind of more
  general logging monad, but for now, there are no monads here.
-}
module Canteven.Log (
  setupLogging
) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (Value(String, Object), (.:?), (.!=), (.:))
import Data.Yaml (FromJSON(parseJSON))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import System.IO (stdout)
import System.Log (Priority(INFO, DEBUG))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (updateGlobalLogger, setHandlers, setLevel)
import qualified Canteven.Config as Config (canteven)
import qualified Data.Text as T


{- |
  Read the program configuration, using the @canteven-config@, and set
  up @hslogger@.
-}
setupLogging :: IO ()
setupLogging =
  installConfig =<< Config.canteven


{- |
  Do all of the things that it takes to get logging set up the way we
  want it.
-}
installConfig :: LoggingConfig -> IO ()
installConfig LoggingConfig {logfile, level = LP level, loggers} = do
  fileHandlers <-
    case logfile of
      Nothing -> return []
      Just filename -> do
        createDirectoryIfMissing True (dropFileName filename)
        file <- tweak <$> fileHandler filename DEBUG
        return [file]

  console <- tweak <$> streamHandler stdout DEBUG
  let handlers = console:fileHandlers
  updateGlobalLogger "" (setLevel level . setHandlers handlers)
  sequence_ [
      updateGlobalLogger loggerName (setLevel loggerLevel) |
      LoggerDetails {loggerName, loggerLevel = LP loggerLevel} <- loggers
    ]
  where
    tweak h = setFormatter h (simpleLogFormatter logFormat)
    logFormat = "$prio [$tid] [$time] $loggername - $msg"


data LoggingConfig =
  LoggingConfig {
    level :: LogPriority,
    logfile :: Maybe FilePath,
    loggers :: [LoggerDetails]
  }

instance FromJSON LoggingConfig where
  parseJSON (Object topLevel) = do
    mLogging <- topLevel .:? "logging"
    case mLogging of
      Nothing -> return defaultLogging
      Just logging -> LoggingConfig
        <$> logging .:? "level" .!= LP INFO
        <*> logging .:? "logfile"
        <*> logging .:? "loggers" .!= []

  parseJSON value =
    fail $ "Couldn't parse logging config from value " ++ show value


{- |
  Don't bother with @data-default@. `LoggingConfig` is not exposed and
  the fewer dependencies the better.
-}
defaultLogging :: LoggingConfig
defaultLogging = LoggingConfig {
    level = LP INFO,
    logfile = Nothing,
    loggers = []
  }


{- |
  A wrapper for Priority, so we can avoid orphan instances
-}
newtype LogPriority = LP Priority

instance FromJSON LogPriority where
  parseJSON (String s) = case reads (T.unpack s) of
    [(priority, "")] -> return (LP priority)
    _ -> fail $ "couldn't parse Priority from string " ++ show s
  parseJSON value = fail $ "Couldn't parse Priority from value " ++ show value


{- |
  A way to set more fined-grained configuration for specific loggers.
-}
data LoggerDetails =
  LoggerDetails {
    loggerName :: String,
    loggerLevel :: LogPriority
  }

instance FromJSON LoggerDetails where
  parseJSON (Object details) = do
    loggerName <- details .: "logger"
    loggerLevel <- details .: "level"
    return LoggerDetails {loggerName, loggerLevel}
  parseJSON value =
    fail $ "Couldn't parse logger details from value " ++ show value


