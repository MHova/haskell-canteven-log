{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canteven.Log.MonadLog (
    LoggerTImpl,
    getCantevenOutput,

    {- Reexports -}
    LoggingConfig(LoggingConfig, level, logfile, loggers),
    LoggerDetails(loggerName, loggerPackage, loggerModule, loggerLevel),
    newLoggingConfig,
    ) where

import Canteven.Log.Types (LoggingConfig(LoggingConfig, logfile,
    level, loggers),
    LoggerDetails(LoggerDetails, loggerName, loggerPackage,
    loggerModule, loggerLevel),
    newLoggingConfig)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LogSource, LogLevel(LevelOther))
import Data.Char (toUpper)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Language.Haskell.TH (Loc(loc_filename, loc_package, loc_module,
    loc_start))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import System.IO (Handle, IOMode(AppendMode), openFile, stdout, hFlush)
import System.Log.FastLogger (LogStr, fromLogStr, toLogStr)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

type LoggerTImpl = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

getCantevenOutput
    :: (MonadIO io)
    => LoggingConfig
    -> io LoggerTImpl
getCantevenOutput config =
    uncurry cantevenOutput <$> liftIO setupLogger
  where
    setupLogger = do
        handle <- setupHandle config
        return (config, handle)

cantevenOutput
    :: LoggingConfig
    -> Handle
    -> Loc
    -> LogSource
    -> LogLevel
    -> LogStr
    -> IO ()
cantevenOutput config handle loc src level msg =
    when (configPermits config loc src level) $ do
    time <- getZonedTime
    threadId <- myThreadId
    S8.hPutStr handle . fromLogStr $ cantevenLogFormat loc src level msg time threadId
    hFlush handle

-- | Figure out whether a particular log message is permitted, given a
-- particular config.
--
-- FIXME: if two LoggerDetails match the same message, it should probably take
-- the answer given by the most specific one that matches. However, at present
-- it just takes the first one.
configPermits :: LoggingConfig -> Loc -> LogSource -> LogLevel -> Bool
configPermits LoggingConfig {level=defaultLP, loggers} = runFilters
  where
    predicates = map toPredicate loggers
    toPredicate LoggerDetails {loggerName, loggerPackage,
                               loggerModule, loggerLevel=loggerLevel}
        loc src level =
        if matches (T.pack <$> loggerName) src &&
           matches loggerPackage (loc_package loc) &&
           matchesGlob loggerModule (loc_module loc)
        then Just (level >= loggerLevel)
        else Nothing
    -- It's considered a "match" if either the specification is absent (matches
    -- everything), or the specification is given and matches the target.
    matches Nothing _ = True
    matches (Just s1) s2 = s1 == s2
    -- Not real glob matching.
    matchesGlob Nothing _ = True
    matchesGlob (Just p) candidate
        | "*" `isSuffixOf` p = dropWhileEnd (=='*') p `isPrefixOf` candidate
        | otherwise = p == candidate
    runFilters loc src level =
        -- default to the defaultLP
        fromMaybe (level >= defaultLP) $
        -- take the first value
        listToMaybe $
        -- of the predicates that returned Just something
        mapMaybe (\p -> p loc src level) predicates


-- | This is similar to the version defined in monad-logger (which we can't
-- share because of privacy restrictions), but with the added nuance of
-- uppercasing.
cantevenLogLevelStr :: LogLevel -> LogStr
cantevenLogLevelStr level = case level of
    LevelOther t -> toLogStr $ T.toUpper t
    _            -> toLogStr $ S8.pack $ map toUpper $ drop 5 $ show level

-- | This log format is inspired by syslog and the X.org log
-- formats. Rationales are:
--
-- * Put the date first, because the date string tends to be a fixed number
--   of characters (or +/- 1 around changes to DST), so the eye can easily
--   skim over them.
--
-- * The "source" of a message comes before the message itself. "Source" is
--   composed of not just the "logger name" (called a source in
--   monad-logger), but also the package/module name and the thread
--   ID. Package and module name might seem controversial, but they
--   correspond to e.g. Log4J logger names based on classes.
--
-- * Filename/position of the message is perhaps the least important, but
--   can still be helpful. Put it at the end.
cantevenLogFormat
    :: Loc
    -> LogSource
    -> LogLevel
    -> LogStr
    -> ZonedTime
    -> ThreadId
    -> LogStr
cantevenLogFormat loc src level msg t tid =
    "[" <> toLogStr (fmtTime t) <> "] " <>
    cantevenLogLevelStr level <>
    " " <>
    (if T.null src
        then mempty
        else toLogStr src) <>
    "@" <> toLogStr (loc_package loc ++ ":" ++ loc_module loc) <>
    "[" <>
    toLogStr (show tid) <> "]: " <>
    msg <> " (" <> toLogStr (S8.pack fileLocStr) <> ")\n"
  where
    fmtTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%q %Z"
    fileLocStr =
        loc_filename loc ++ ':' : line loc ++ ':' : char loc
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

openFileForLogging :: FilePath -> IO Handle
openFileForLogging filename = do
    createDirectoryIfMissing True (dropFileName filename)
    openFile filename AppendMode

setupHandle :: LoggingConfig -> IO Handle
setupHandle LoggingConfig {logfile} =
    maybe (return stdout) openFileForLogging logfile
