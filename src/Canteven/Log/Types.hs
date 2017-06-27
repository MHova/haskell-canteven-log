{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canteven.Log.Types (
    LoggerDetails(..),
    LoggingConfig(..),
    defaultLogging,
    newLoggingConfig,
    ) where

import Control.Monad.Logger (LogLevel(LevelDebug, LevelInfo, LevelWarn,
    LevelError, LevelOther))
import Data.Aeson (Value(String, Object), (.:?), (.!=), (.:))
import Data.Maybe (catMaybes, listToMaybe)
import Data.Yaml (FromJSON(parseJSON))
import Data.Text (pack)

{-
  A convenience function for creating a LoggingConfig when you just want a
  default LoggingConfig with the specified LogLevel in String form. Useful
  for services that read configuration directly from environment variables
  instead of a configuration file.
-}
newLoggingConfig :: String -> LoggingConfig
newLoggingConfig s = defaultLogging {level = unLP $ read s}

data LoggingConfig =
  LoggingConfig {
    level :: LogLevel,
    logfile :: Maybe FilePath,
    loggers :: [LoggerDetails]
  }

instance FromJSON LoggingConfig where
  parseJSON (Object topLevel) = do
    mLogging <- topLevel .:? "logging"
    case mLogging of
      Nothing -> return defaultLogging
      Just logging -> LoggingConfig
        <$> (unLP <$> (logging .:? "level" .!= LP LevelInfo))
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
    level = LevelInfo,
    logfile = Nothing,
    loggers = []
  }


{- |
  A wrapper for LogLevel, so we can avoid orphan instances
-}
newtype LogPriority = LP {unLP :: LogLevel}

instance FromJSON LogPriority where
  parseJSON (String "DEBUG" ) = return (LP LevelDebug)
  parseJSON (String "INFO" ) = return (LP LevelInfo)
  parseJSON (String "WARN" ) = return (LP LevelWarn)
  parseJSON (String "WARNING" ) = return (LP LevelWarn)
  parseJSON (String "ERROR" ) = return (LP LevelError)
  parseJSON (String s) = return (LP (LevelOther s))
  parseJSON value = fail $ "Couldn't parse LogLevel from value " ++ show value

{- The provided dervived Read instance for LogLevel isn't very useful -}
instance Read LogPriority where
  readsPrec _ "DEBUG" = [(LP LevelDebug, "")]
  readsPrec _ "INFO" = [(LP LevelInfo, "")]
  readsPrec _ "WARN" = [(LP LevelWarn, "")]
  readsPrec _ "WARNING" = [(LP LevelWarn, "")]
  readsPrec _ "ERROR" = [(LP LevelError, "")]
  readsPrec _ other = [(LP . LevelOther $ pack other, "")]

{- |
  A way to set more fined-grained configuration for specific log messages.

  Name, package, and module are "selectors" that identify which messages should
  be configured. Any absent "selectors" match everything. Name and package have to
  match exactly. Module can either match exactly, or -- if the config specifies a
  module ending in an asterisk -- match a prefix.

  'loggerLevel' is a "minimum priority". Messages that aren't at least as severe
  as this will not be logged.
-}
data LoggerDetails =
  LoggerDetails {
    loggerName :: Maybe String,
    loggerPackage :: Maybe String,
    loggerModule :: Maybe String,
    loggerLevel :: LogLevel
  }

instance FromJSON LoggerDetails where
  parseJSON (Object details) = do
    loggerName <- do
        names <- catMaybes <$> sequence [
            details .:? "logger",
            details .:? "source",
            details .:? "name"]
        return $ listToMaybe names
    loggerLevel <- unLP <$> details .: "level"
    loggerModule <- details .:? "module"
    loggerPackage <- details .:? "package"
    return LoggerDetails {loggerName, loggerPackage, loggerModule, loggerLevel}
  parseJSON value =
    fail $ "Couldn't parse logger details from value " ++ show value
