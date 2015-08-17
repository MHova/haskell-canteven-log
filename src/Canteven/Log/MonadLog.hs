{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Canteven.Log.MonadLog (
    cantevenOutput,
    getRunCantevenLoggingT,
    runCantevenLoggingDefaultT,
    ) where

import Canteven.Config (canteven)
import Canteven.Log.Types (LoggingConfig(LoggingConfig, logfile),
    defaultLogging)
import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LogSource, LogLevel(LevelOther),
    LoggingT(runLoggingT))
import Data.Char (toUpper)
import Data.Monoid ((<>), mempty)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Language.Haskell.TH (Loc(loc_filename, loc_package, loc_module,
    loc_start))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)
import System.IO (Handle, IOMode(AppendMode), openFile, stdout)
import System.Log.FastLogger (LogStr, fromLogStr, toLogStr)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

runCantevenLoggingT
    :: (MonadIO io)
    => LoggingConfig
    -> LoggingT io a
    -> io a
runCantevenLoggingT config = (`runLoggingT` cantevenOutput config)

-- | Get the logging config using canteven, and produce a way to use that
-- logging config to run a LoggingT.
getRunCantevenLoggingT
    :: (Functor io1, MonadIO io1, MonadIO io2)
    => io1 (LoggingT io2 a -> io2 a)
getRunCantevenLoggingT =
    runCantevenLoggingT <$> liftIO canteven

-- | Run a LoggingT, using the canteven logging format, with the default logging
-- configuration.
runCantevenLoggingDefaultT
    :: (MonadIO io)
    => LoggingT io a
    -> io a
runCantevenLoggingDefaultT = runCantevenLoggingT defaultLogging

cantevenOutput
    :: LoggingConfig
    -> Loc
    -> LogSource
    -> LogLevel
    -> LogStr
    -> IO ()
cantevenOutput LoggingConfig {logfile} loc src level msg = do
    time <- getCurrentTime
    threadId <- myThreadId
    handle <- maybe (return stdout) openFileForLogging logfile
    S8.hPutStr handle . fromLogStr $ cantevenLogFormat loc src level msg time threadId

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
    -> UTCTime
    -> ThreadId
    -> LogStr
cantevenLogFormat loc src level msg t tid =
    "[" <> toLogStr (show t) <> "] " <>
    cantevenLogLevelStr level <>
    " " <>
    (if T.null src
        then mempty
        else toLogStr src) <>
    "@" <> toLogStr (loc_module loc) <> "[" <>
    toLogStr (show tid) <> "]: " <>
    msg <> " (" <> toLogStr (S8.pack fileLocStr) <> ")\n"
  where
    fileLocStr = loc_package loc ++
      ' ' : loc_filename loc ++ ':' : line loc ++ ':' : char loc
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

openFileForLogging :: FilePath -> IO Handle
openFileForLogging filename = do
    createDirectoryIfMissing True (dropFileName filename)
    openFile filename AppendMode
