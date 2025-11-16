{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : AgenticFramework.Logging
Description : Observability and structured logging for agents
Copyright   : (c) 2025
License     : MIT

This module provides comprehensive logging and observability for agent execution.
It integrates with monad-logger for ecosystem compatibility while also supporting
custom log handlers for flexible backends.

= Features

* Structured logging with typed metadata
* Integration with monad-logger (FR-029a)
* Pluggable custom handlers (FR-029b)
* Filtering by agent, tool, level, and time
* JSON and text export formats

= Usage

@
import AgenticFramework.Logging
import Control.Monad.Logger

main :: IO ()
main = runStdoutLoggingT $ do
  logInfoN "Starting agent execution"
  result <- executeAgent myAgent "task"
  logDebugN $ "Result: " <> show result
@

-}

module AgenticFramework.Logging
  ( -- * Logging Monad
    LoggingT
  , runLoggingT
  , runStdoutLoggingT

    -- * Log Handler Typeclass
  , LogHandler (..)

    -- * Log Entry Types
  , LogEntry (..)
  , LogLevel (..)
  , ExecutionLog (..)

    -- * Logging Functions
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logWithMetadata
  , logToolExecution
  , logAgentReasoning

    -- * Filtering
  , LogFilter (..)
  , filterLogs
  , defaultFilter

    -- * Export
  , exportLogsJSON
  , exportLogsText

  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, runLoggingT)
import qualified Control.Monad.Logger as Logger
import Data.Aeson (ToJSON, FromJSON, encode)
import qualified Data.Aeson as Aeson
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy as BSL

import AgenticFramework.Types
import qualified AgenticFramework.Types as Types

--------------------------------------------------------------------------------
-- Execution Log
--------------------------------------------------------------------------------

-- | Complete log of agent execution with all entries.
data ExecutionLog = ExecutionLog
  { execLogId :: UUID                     -- ^ Unique log identifier
  , execLogAgentId :: AgentId             -- ^ Agent that produced this log
  , execLogEntries :: Seq LogEntry        -- ^ Chronological log entries
  , execLogStartTime :: UTCTime           -- ^ When execution started
  , execLogEndTime :: Maybe UTCTime       -- ^ When execution ended (if completed)
  } deriving (Show, Eq)

instance ToJSON ExecutionLog where
  toJSON log = Aeson.object
    [ "id" Aeson..= execLogId log
    , "agent_id" Aeson..= execLogAgentId log
    , "entries" Aeson..= execLogEntries log
    , "start_time" Aeson..= execLogStartTime log
    , "end_time" Aeson..= execLogEndTime log
    ]

instance FromJSON ExecutionLog where
  parseJSON = Aeson.withObject "ExecutionLog" $ \v -> ExecutionLog
    <$> v Aeson..: "id"
    <*> v Aeson..: "agent_id"
    <*> v Aeson..: "entries"
    <*> v Aeson..: "start_time"
    <*> v Aeson..: "end_time"

--------------------------------------------------------------------------------
-- Log Handler Typeclass
--------------------------------------------------------------------------------

-- | Typeclass for custom log handlers.
--
-- Implement this to create custom logging backends (files, databases,
-- monitoring systems, etc.)
--
-- __Example:__
--
-- @
-- data FileLogHandler = FileLogHandler FilePath
--
-- instance LogHandler FileLogHandler where
--   logEntry (FileLogHandler path) entry = do
--     let formatted = formatLogEntry entry
--     appendFile path (T.unpack formatted <> "\\n")
-- @
--
class LogHandler h where
  -- | Write a log entry using this handler.
  logEntry :: h -> LogEntry -> IO ()

--------------------------------------------------------------------------------
-- Logging Functions
--------------------------------------------------------------------------------

-- | Log a debug-level message.
logDebug :: MonadIO m => Text -> m ()
logDebug msg = liftIO $ do
  timestamp <- getCurrentTime
  let entry = LogEntry
        { Types.logLevel = DEBUG
        , Types.logTimestamp = timestamp
        , Types.logAgentId = Nothing
        , Types.logToolName = Nothing
        , Types.logMessage = msg
        , Types.logMetadata = Aeson.Null
        , Types.logCallStack = []
        }
  -- In actual implementation, this would route to configured handlers
  return ()

-- | Log an info-level message.
logInfo :: MonadIO m => Text -> m ()
logInfo msg = liftIO $ do
  timestamp <- getCurrentTime
  let entry = LogEntry
        { Types.logLevel = INFO
        , Types.logTimestamp = timestamp
        , Types.logAgentId = Nothing
        , Types.logToolName = Nothing
        , Types.logMessage = msg
        , Types.logMetadata = Aeson.Null
        , Types.logCallStack = []
        }
  return ()

-- | Log a warning-level message.
logWarn :: MonadIO m => Text -> m ()
logWarn msg = liftIO $ do
  timestamp <- getCurrentTime
  let entry = LogEntry
        { Types.logLevel = WARN
        , Types.logTimestamp = timestamp
        , Types.logAgentId = Nothing
        , Types.logToolName = Nothing
        , Types.logMessage = msg
        , Types.logMetadata = Aeson.Null
        , Types.logCallStack = []
        }
  return ()

-- | Log an error-level message.
logError :: MonadIO m => Text -> m ()
logError msg = liftIO $ do
  timestamp <- getCurrentTime
  let entry = LogEntry
        { Types.logLevel = ERROR
        , Types.logTimestamp = timestamp
        , Types.logAgentId = Nothing
        , Types.logToolName = Nothing
        , Types.logMessage = msg
        , Types.logMetadata = Aeson.Null
        , Types.logCallStack = []
        }
  return ()

-- | Log a message with additional structured metadata.
--
-- __Example:__
--
-- @
-- logWithMetadata INFO "Tool executed" $ Aeson.object
--   [ "tool_name" .= "readFile"
--   , "duration_ms" .= (150 :: Int)
--   , "success" .= True
--   ]
-- @
--
logWithMetadata :: MonadIO m => LogLevel -> Text -> Aeson.Value -> m ()
logWithMetadata level msg metadata = liftIO $ do
  timestamp <- getCurrentTime
  let entry = LogEntry
        { Types.logLevel = level
        , Types.logTimestamp = timestamp
        , Types.logAgentId = Nothing
        , Types.logToolName = Nothing
        , Types.logMessage = msg
        , Types.logMetadata = metadata
        , Types.logCallStack = []
        }
  return ()

-- | Log tool execution with input, output, and timing information (FR-025).
--
-- __Example:__
--
-- @
-- logToolExecution "readFile" agentId (ToolInput inputData) (Right (ToolOutput outputData)) 150.5
-- @
--
logToolExecution
  :: MonadIO m
  => Text                              -- ^ Tool name
  -> Maybe AgentId                     -- ^ Agent ID (if known)
  -> ToolInput                         -- ^ Tool input
  -> Either ToolError ToolOutput       -- ^ Tool output or error
  -> Double                            -- ^ Duration in milliseconds
  -> m ()
logToolExecution toolName agentId (ToolInput input) result durationMs = liftIO $ do
  timestamp <- getCurrentTime
  let level = case result of
        Left _ -> ERROR
        Right _ -> INFO
  let metadata = Aeson.object
        [ "tool_name" Aeson..= toolName
        , "input" Aeson..= input
        , "output" Aeson..= case result of
            Left err -> Aeson.object ["error" Aeson..= show err]
            Right (ToolOutput out) -> out
        , "duration_ms" Aeson..= durationMs
        , "success" Aeson..= case result of
            Left _ -> False
            Right _ -> True
        ]
  let entry = LogEntry
        { Types.logLevel = level
        , Types.logTimestamp = timestamp
        , Types.logAgentId = agentId
        , Types.logToolName = Just toolName
        , Types.logMessage = "Tool executed: " <> toolName
        , Types.logMetadata = metadata
        , Types.logCallStack = []
        }
  -- TODO: Route to configured log handlers
  return ()

-- | Log agent reasoning and thought processes from LLM (FR-026, FR-027).
--
-- __Example:__
--
-- @
-- logAgentReasoning agentId "I need to check the file contents before writing" "planning" Nothing
-- @
--
logAgentReasoning
  :: MonadIO m
  => AgentId                           -- ^ Agent ID
  -> Text                              -- ^ Reasoning text (thought process)
  -> Text                              -- ^ Reasoning type (e.g., "planning", "reflection", "tool_selection")
  -> Maybe Text                        -- ^ Optional context or related tool
  -> m ()
logAgentReasoning agentId reasoning reasoningType maybeContext = liftIO $ do
  timestamp <- getCurrentTime
  let metadata = Aeson.object $
        [ "reasoning_type" Aeson..= reasoningType
        , "reasoning" Aeson..= reasoning
        ] ++ case maybeContext of
               Nothing -> []
               Just ctx -> ["context" Aeson..= ctx]
  let entry = LogEntry
        { Types.logLevel = DEBUG
        , Types.logTimestamp = timestamp
        , Types.logAgentId = Just agentId
        , Types.logToolName = Nothing
        , Types.logMessage = "[Reasoning] " <> reasoning
        , Types.logMetadata = metadata
        , Types.logCallStack = []
        }
  -- TODO: Route to configured log handlers
  return ()

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

-- | Filter criteria for log entries.
data LogFilter = LogFilter
  { filterAgentId :: Maybe AgentId      -- ^ Filter by agent ID
  , filterToolName :: Maybe Text        -- ^ Filter by tool name
  , filterMinLevel :: LogLevel          -- ^ Minimum log level
  , filterTimeRange :: Maybe (UTCTime, UTCTime)  -- ^ Time range filter
  , filterHasError :: Bool              -- ^ Only show errors
  } deriving (Show, Eq)

-- | Default filter (no filtering).
defaultFilter :: LogFilter
defaultFilter = LogFilter
  { filterAgentId = Nothing
  , filterToolName = Nothing
  , filterMinLevel = DEBUG
  , filterTimeRange = Nothing
  , filterHasError = False
  }

-- | Filter log entries based on criteria.
--
-- __Example:__
--
-- @
-- -- Get only ERROR level logs from a specific agent
-- let filtered = filterLogs executionLog $ defaultFilter
--       { filterAgentId = Just myAgentId
--       , filterMinLevel = ERROR
--       }
-- @
--
filterLogs :: ExecutionLog -> LogFilter -> [LogEntry]
filterLogs log filter' =
  let entries = toList $ execLogEntries log
  in Prelude.filter (matchesFilter filter') entries

-- | Check if a log entry matches the filter criteria.
matchesFilter :: LogFilter -> LogEntry -> Bool
matchesFilter filter' entry =
  matchesAgentId && matchesToolName && matchesLevel && matchesTimeRange && matchesError
  where
    matchesAgentId = case filterAgentId filter' of
      Nothing -> True
      Just aid -> Types.logAgentId entry == Just aid

    matchesToolName = case filterToolName filter' of
      Nothing -> True
      Just name -> Types.logToolName entry == Just name

    matchesLevel = Types.logLevel entry >= filterMinLevel filter'

    matchesTimeRange = case filterTimeRange filter' of
      Nothing -> True
      Just (start, end) ->
        Types.logTimestamp entry >= start && Types.logTimestamp entry <= end

    matchesError = not (filterHasError filter') || Types.logLevel entry == ERROR

--------------------------------------------------------------------------------
-- Export Functions
--------------------------------------------------------------------------------

-- | Export execution log as JSON.
--
-- Returns a JSON-encoded bytestring suitable for storage or transmission.
--
exportLogsJSON :: ExecutionLog -> BSL.ByteString
exportLogsJSON = encode

-- | Export execution log as human-readable text.
--
-- Formats log entries in a readable format with timestamps, levels,
-- and messages.
--
-- __Example Output:__
--
-- @
-- [2025-11-15 10:30:45 INFO] Agent started
-- [2025-11-15 10:30:46 DEBUG] Tool: readFile - Reading config.json
-- [2025-11-15 10:30:47 ERROR] Tool: readFile - File not found
-- @
--
exportLogsText :: ExecutionLog -> Text
exportLogsText log =
  let entries = toList $ execLogEntries log
      formatted = map formatLogEntry entries
  in T.intercalate "\n" formatted

-- | Format a single log entry as text.
formatLogEntry :: LogEntry -> Text
formatLogEntry entry =
  let timestamp = T.pack $ show $ Types.logTimestamp entry
      level = T.pack $ show $ Types.logLevel entry
      msg = Types.logMessage entry
      toolInfo = case Types.logToolName entry of
        Nothing -> ""
        Just tool -> "Tool: " <> tool <> " - "
  in "[" <> timestamp <> " " <> level <> "] " <> toolInfo <> msg
