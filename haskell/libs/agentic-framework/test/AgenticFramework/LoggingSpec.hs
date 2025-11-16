{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : AgenticFramework.LoggingSpec
Description : Unit tests for logging functionality
Copyright   : (c) 2025
License     : MIT

Unit tests for the logging module, including:

* Log handler behavior
* Filtering functionality
* Export format correctness
* Structured logging with metadata

-}

module AgenticFramework.LoggingSpec (spec) where

import Test.Hspec
import Data.Time (getCurrentTime, UTCTime, addUTCTime)
import Data.UUID (nil)
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL

import AgenticFramework.Types
import qualified AgenticFramework.Types as Types
import AgenticFramework.Logging
import AgenticFramework.Logging.Handlers
import System.Console.ANSI.Types (Color(..))

--------------------------------------------------------------------------------
-- Test Specs
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Logging" $ do

  describe "Log Entry Creation" $ do
    it "creates log entry with correct level" $ do
      timestamp <- getCurrentTime
      let entry = LogEntry
            { logLevel = INFO
            , logTimestamp = timestamp
            , logAgentId = Nothing
            , logToolName = Nothing
            , logMessage = "Test message"
            , logMetadata = Aeson.Null
            , logCallStack = []
            }
      logLevel entry `shouldBe` INFO
      logMessage entry `shouldBe` "Test message"

    it "includes agent ID when provided" $ do
      timestamp <- getCurrentTime
      let agentId = AgentId nil
          entry = LogEntry
            { logLevel = DEBUG
            , logTimestamp = timestamp
            , logAgentId = Just agentId
            , logToolName = Nothing
            , logMessage = "Agent log"
            , logMetadata = Aeson.Null
            , logCallStack = []
            }
      logAgentId entry `shouldBe` Just agentId

    it "includes tool name when provided" $ do
      timestamp <- getCurrentTime
      let entry = LogEntry
            { logLevel = INFO
            , logTimestamp = timestamp
            , logAgentId = Nothing
            , logToolName = Just "readFile"
            , logMessage = "Tool execution"
            , logMetadata = Aeson.Null
            , logCallStack = []
            }
      logToolName entry `shouldBe` Just "readFile"

  describe "ExecutionLog" $ do
    it "creates execution log with entries" $ do
      startTime <- getCurrentTime
      timestamp1 <- getCurrentTime
      timestamp2 <- return $ addUTCTime 1 timestamp1

      let entry1 = createTestEntry INFO timestamp1 "First log"
          entry2 = createTestEntry DEBUG timestamp2 "Second log"
          agentId = AgentId nil
          log = ExecutionLog
            { execLogId = nil
            , execLogAgentId = agentId
            , execLogEntries = Seq.fromList [entry1, entry2]
            , execLogStartTime = startTime
            , execLogEndTime = Nothing
            }

      Seq.length (execLogEntries log) `shouldBe` 2
      execLogAgentId log `shouldBe` agentId

  describe "Filtering" $ do
    it "filters by log level" $ do
      now <- getCurrentTime
      let entry1 = createTestEntry DEBUG now "Debug message"
          entry2 = createTestEntry INFO now "Info message"
          entry3 = createTestEntry ERROR now "Error message"
          log = createTestLog [entry1, entry2, entry3]
          filter' = defaultFilter { filterMinLevel = INFO }
          filtered = filterLogs log filter'

      length filtered `shouldBe` 2
      map Types.logLevel filtered `shouldBe` [INFO, ERROR]

    it "filters by agent ID" $ do
      now <- getCurrentTime
      let agentId1 = AgentId nil
          entry1 = (createTestEntry INFO now "Agent 1") { logAgentId = Just agentId1 }
          entry2 = createTestEntry INFO now "No agent"
          log = createTestLog [entry1, entry2]
          filter' = defaultFilter { filterAgentId = Just agentId1 }
          filtered = filterLogs log filter'

      length filtered `shouldBe` 1

    it "filters by tool name" $ do
      now <- getCurrentTime
      let entry1 = (createTestEntry INFO now "Tool log") { logToolName = Just "readFile" }
          entry2 = createTestEntry INFO now "No tool"
          log = createTestLog [entry1, entry2]
          filter' = defaultFilter { filterToolName = Just "readFile" }
          filtered = filterLogs log filter'

      length filtered `shouldBe` 1

    it "filters by error flag" $ do
      now <- getCurrentTime
      let entry1 = createTestEntry ERROR now "Error 1"
          entry2 = createTestEntry INFO now "Info"
          entry3 = createTestEntry ERROR now "Error 2"
          log = createTestLog [entry1, entry2, entry3]
          filter' = defaultFilter { filterHasError = True }
          filtered = filterLogs log filter'

      length filtered `shouldBe` 2
      all (\e -> Types.logLevel e == ERROR) filtered `shouldBe` True

  describe "Export" $ do
    it "exports to JSON" $ do
      now <- getCurrentTime
      let entry = createTestEntry INFO now "Test message"
          log = createTestLog [entry]
          json = exportLogsJSON log

      BSL.length json `shouldSatisfy` (> 0)

    it "exports to text format" $ do
      now <- getCurrentTime
      let entry1 = createTestEntry INFO now "First message"
          entry2 = createTestEntry DEBUG now "Second message"
          log = createTestLog [entry1, entry2]
          text = exportLogsText log

      T.isInfixOf "First message" text `shouldBe` True
      T.isInfixOf "Second message" text `shouldBe` True
      T.isInfixOf "INFO" text `shouldBe` True
      T.isInfixOf "DEBUG" text `shouldBe` True

  describe "Colorization" $ do
    it "colorizes DEBUG as cyan" $ do
      let (color, bold) = colorForLevel DEBUG
      color `shouldBe` Cyan
      bold `shouldBe` False

    it "colorizes INFO as green" $ do
      let (color, bold) = colorForLevel INFO
      color `shouldBe` Green
      bold `shouldBe` False

    it "colorizes WARN as yellow" $ do
      let (color, bold) = colorForLevel WARN
      color `shouldBe` Yellow
      bold `shouldBe` False

    it "colorizes ERROR as red bold" $ do
      let (color, bold) = colorForLevel ERROR
      color `shouldBe` Red
      bold `shouldBe` True

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

createTestEntry :: LogLevel -> UTCTime -> T.Text -> LogEntry
createTestEntry level timestamp msg = LogEntry
  { Types.logLevel = level
  , Types.logTimestamp = timestamp
  , Types.logAgentId = Nothing
  , Types.logToolName = Nothing
  , Types.logMessage = msg
  , Types.logMetadata = Aeson.Null
  , Types.logCallStack = []
  }

createTestLog :: [LogEntry] -> ExecutionLog
createTestLog entries =
  case entries of
    (firstEntry:_) -> ExecutionLog
      { execLogId = nil
      , execLogAgentId = AgentId nil
      , execLogEntries = Seq.fromList entries
      , execLogStartTime = Types.logTimestamp firstEntry
      , execLogEndTime = Nothing
      }
    [] -> error "createTestLog: empty entry list"
