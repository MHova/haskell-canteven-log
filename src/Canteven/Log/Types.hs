{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canteven.Log.Types (
    LogPriority(..),
    LoggerDetails(..),
    LoggingConfig(..),
    defaultLogging,
    ) where

import Data.Aeson (Value(String, Object), (.:?), (.!=), (.:))
import Data.Yaml (FromJSON(parseJSON))
import Control.Applicative ((<$>), (<*>))
import System.Log (Priority(INFO))
import qualified Data.Text as T

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
