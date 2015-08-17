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

import Canteven.Log.Types (LogPriority(LP),
    LoggingConfig(LoggingConfig, logfile, level, loggers),
    LoggerDetails(LoggerDetails, loggerName, loggerLevel))
import Control.Applicative ((<$>))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import System.IO (stdout)
import System.Log (Priority(DEBUG))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (updateGlobalLogger, setHandlers, setLevel)
import qualified Canteven.Config as Config (canteven)


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
    logFormat = "[$time] $prio $loggername[$tid]: $msg"
