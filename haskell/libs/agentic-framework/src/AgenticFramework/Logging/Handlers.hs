{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : AgenticFramework.Logging.Handlers
Description : Built-in log handlers (colorized stdout, file logging)
Copyright   : (c) 2025
License     : MIT

This module provides ready-to-use log handler implementations:

* 'ColorizedStdoutHandler' - Console output with ANSI colors
* 'FileHandler' - Append logs to a file

= Usage

@
import AgenticFramework.Logging
import AgenticFramework.Logging.Handlers

main :: IO ()
main = do
  -- Colorized console output
  let stdoutHandler = colorizedStdoutHandler

  -- File logging
  fileHandler <- fileHandler "agent.log"

  -- Use handlers
  logEntry stdoutHandler myLogEntry
  logEntry fileHandler myLogEntry
@

-}

module AgenticFramework.Logging.Handlers
  ( -- * Handlers
    ColorizedStdoutHandler
  , colorizedStdoutHandler
  , FileHandler
  , fileHandler

    -- * Utilities
  , colorize
  , colorForLevel

  ) where

import AgenticFramework.Types (LogLevel(..), LogEntry(..))
import AgenticFramework.Logging (LogHandler(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.ANSI
import System.IO (Handle, IOMode(..), openFile, hPutStrLn, hFlush)

--------------------------------------------------------------------------------
-- Colorized Stdout Handler
--------------------------------------------------------------------------------

-- | Log handler that writes colorized output to stdout.
data ColorizedStdoutHandler = ColorizedStdoutHandler

-- | Create a colorized stdout handler.
colorizedStdoutHandler :: ColorizedStdoutHandler
colorizedStdoutHandler = ColorizedStdoutHandler

instance LogHandler ColorizedStdoutHandler where
  logEntry ColorizedStdoutHandler entry = do
    let timestamp = T.pack $ show $ logTimestamp entry
        level = logLevel entry
        msg = logMessage entry
        toolInfo = case logToolName entry of
          Nothing -> ""
          Just tool -> "Tool: " <> tool <> " - "

    -- Format: [timestamp LEVEL] message
    let formatted = "[" <> timestamp <> " " <> T.pack (show level) <> "] " <> toolInfo <> msg
    let colored = colorize level formatted

    TIO.putStrLn colored

--------------------------------------------------------------------------------
-- File Handler
--------------------------------------------------------------------------------

-- | Log handler that appends to a file.
data FileHandler = FileHandler FilePath Handle

-- | Create a file handler that appends to the specified file.
--
-- The file will be created if it doesn't exist, and logs will be
-- appended to preserve existing content.
--
fileHandler :: FilePath -> IO FileHandler
fileHandler path = do
  handle <- openFile path AppendMode
  return $ FileHandler path handle

instance LogHandler FileHandler where
  logEntry (FileHandler _ handle) entry = do
    let timestamp = T.pack $ show $ logTimestamp entry
        level = T.pack $ show $ logLevel entry
        msg = logMessage entry
        toolInfo = case logToolName entry of
          Nothing -> ""
          Just tool -> "Tool: " <> tool <> " - "

    -- Format: [timestamp LEVEL] message (no colors in file)
    let formatted = "[" <> timestamp <> " " <> level <> "] " <> toolInfo <> msg

    hPutStrLn handle (T.unpack formatted)
    hFlush handle

--------------------------------------------------------------------------------
-- Color Utilities
--------------------------------------------------------------------------------

-- | Colorize text based on log level.
--
-- * DEBUG - Cyan
-- * INFO - Green
-- * WARN - Yellow
-- * ERROR - Red (bold)
--
colorize :: LogLevel -> Text -> Text
colorize level text =
  let (color, bold) = colorForLevel level
      prefix = T.pack $ setSGRCode [SetColor Foreground Vivid color] ++ if bold then setSGRCode [SetConsoleIntensity BoldIntensity] else ""
      suffix = T.pack $ setSGRCode [Reset]
  in prefix <> text <> suffix

-- | Get ANSI color and bold flag for a log level.
colorForLevel :: LogLevel -> (Color, Bool)
colorForLevel DEBUG = (Cyan, False)
colorForLevel INFO = (Green, False)
colorForLevel WARN = (Yellow, False)
colorForLevel ERROR = (Red, True)
