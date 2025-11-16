{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Types
-- Description : Core type definitions for the Agentic Framework
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module contains all the core type definitions used throughout
-- the Agentic Framework library. These types form the foundation for
-- agents, tools, skills, context management, and orchestration.
--
-- = Type Categories
--
-- * Agent Types: 'AgentId', 'LLMProvider', 'LLMConfig'
-- * Tool Types: 'ToolInput', 'ToolOutput', 'ToolError'
-- * Skill Types: 'SkillLabel', 'SkillCategory'
-- * Context Types: 'TokenMetrics', 'Message', 'ToolExecution'
-- * Orchestration Types: 'ExecutionStatus', 'ErrorPolicy', 'AggregationStrategy'
-- * Logging Types: 'LogLevel', 'LogEntry'
module AgenticFramework.Types
  ( -- * Agent Types
    AgentId (..),
    LLMProvider (..),
    LLMConfig (..),

    -- * Tool Types
    Tool (..),
    ToolSchema (..),
    ToolConfig (..),
    ToolInput (..),
    ToolOutput (..),
    ToolError (..),

    -- * Skill Types
    SkillLabel (..),
    SkillCategory (..),

    -- * Context Types
    TokenMetrics (..),
    Message (..),
    ToolExecution (..),

    -- * Orchestration Types
    ExecutionStatus (..),
    ErrorPolicy (..),
    AggregationStrategy (..),

    -- * Logging Types
    LogLevel (..),
    LogEntry (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Agent Types
--------------------------------------------------------------------------------

-- | Unique identifier for an agent.
--   Agents are identified by UUIDs to ensure global uniqueness.
newtype AgentId = AgentId {unAgentId :: UUID}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Supported LLM providers for agent execution.
data LLMProvider
  = -- | Local Ollama instance
    Ollama
  | -- | OpenAI API (GPT models)
    OpenAI
  | -- | Anthropic Claude API
    Claude
  | -- | HuggingFace Inference API
    HuggingFace
  | -- | Moonshot AI Kimi API
    Kimi
  | -- | Custom provider with name
    Custom Text
  deriving (Show, Eq, Generic)

instance ToJSON LLMProvider

instance FromJSON LLMProvider

-- | Configuration for LLM provider integration.
data LLMConfig = LLMConfig
  { llmProvider :: LLMProvider,
    -- | Model name (e.g., "gpt-4", "claude-3-opus")
    llmModel :: Text,
    -- | API key (if Nothing, retrieve from env)
    llmApiKey :: Maybe Text,
    -- | Base URL for custom endpoints
    llmBaseUrl :: Maybe Text,
    -- | Model's context window size
    llmMaxTokens :: Int,
    -- | Sampling temperature (default 0.7)
    llmTemperature :: Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON LLMConfig

instance FromJSON LLMConfig

--------------------------------------------------------------------------------
-- Tool Types
--------------------------------------------------------------------------------

-- | Input parameters for tool execution.
--   Tools receive JSON values as input (typically JSON objects).
newtype ToolInput = ToolInput {unToolInput :: AesonTypes.Value}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Output from tool execution.
--   Tools return JSON values as output (typically JSON objects).
newtype ToolOutput = ToolOutput {unToolOutput :: AesonTypes.Value}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Errors that can occur during tool execution.
data ToolError
  = -- | General execution failure with message
    ToolExecutionError Text
  | -- | Tool exceeded timeout limit
    ToolTimeoutError
  | -- | Input/output validation failed
    ToolValidationError Text
  | -- | Tool not authorized for this agent
    ToolAuthorizationError
  deriving (Show, Eq, Generic)

instance ToJSON ToolError

instance FromJSON ToolError

-- | A tool that an agent can execute.
data Tool = Tool
  { toolName :: Text,
    toolDescription :: Text,
    toolSchema :: ToolSchema,
    toolExecute :: ToolInput -> IO (Either ToolError ToolOutput),
    -- | Microseconds; Nothing = 10s default
    toolTimeout :: Maybe Int,
    -- | Default True (supports 3 retries)
    toolRetryable :: Bool
  }
  deriving (Generic)

instance Show Tool where
  show t = "Tool {toolName = " ++ show (toolName t) ++ "}"

-- | Eq instance compares tools by name (since function equality is undecidable)
instance Eq Tool where
  t1 == t2 = toolName t1 == toolName t2

-- | JSON Schema for tool input and output validation.
data ToolSchema = ToolSchema
  { -- | JSON Schema for input validation
    inputSchema :: AesonTypes.Value,
    -- | JSON Schema for output validation
    outputSchema :: AesonTypes.Value
  }
  deriving (Show, Eq, Generic)

-- | Configuration for creating a new tool.
data ToolConfig = ToolConfig
  { toolConfigName :: Text,
    toolConfigDescription :: Text,
    toolConfigInputSchema :: AesonTypes.Value,
    toolConfigOutputSchema :: AesonTypes.Value,
    toolConfigExecute :: ToolInput -> IO (Either ToolError ToolOutput),
    toolConfigTimeout :: Maybe Int,
    toolConfigRetryable :: Bool
  }

--------------------------------------------------------------------------------
-- Skill Types
--------------------------------------------------------------------------------

-- | Label identifying a skill.
--   Labels are unique within a skills directory.
newtype SkillLabel = SkillLabel {unSkillLabel :: Text}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Category for organizing skills.
newtype SkillCategory = SkillCategory {unSkillCategory :: Text}
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Context Types
--------------------------------------------------------------------------------

-- | Token usage metrics for context window management.
data TokenMetrics = TokenMetrics
  { -- | Current token usage
    currentTokenCount :: Int,
    -- | Model's maximum token limit
    modelTokenLimit :: Int,
    -- | Percentage used (0.0 to 100.0)
    percentageUsed :: Double,
    -- | Whether summarization has occurred
    summarizationTriggered :: Bool,
    -- | Timestamp of last summarization
    lastSummarization :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON TokenMetrics

instance FromJSON TokenMetrics

-- | Messages in an agent's conversation history.
data Message
  = UserMessage
      { messageContent :: Text,
        messageTimestamp :: UTCTime
      }
  | AssistantMessage
      { messageContent :: Text,
        messageTimestamp :: UTCTime
      }
  | SystemMessage
      { messageContent :: Text,
        messageTimestamp :: UTCTime
      }
  | ToolMessage
      { messageTool :: Text,
        messageResult :: ToolOutput,
        messageTimestamp :: UTCTime
      }
  deriving (Show, Eq, Generic)

instance ToJSON Message

instance FromJSON Message

-- | Record of a tool execution with timing and results.
data ToolExecution = ToolExecution
  { toolExecName :: Text,
    toolExecInput :: ToolInput,
    toolExecOutput :: Either ToolError ToolOutput,
    toolExecTimestamp :: UTCTime,
    toolExecDuration :: NominalDiffTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolExecution where
  toJSON te =
    Aeson.object
      [ "name" Aeson..= toolExecName te,
        "input" Aeson..= toolExecInput te,
        "output" Aeson..= case toolExecOutput te of
          Left err -> Aeson.object ["error" Aeson..= err]
          Right out -> Aeson.object ["success" Aeson..= out],
        "timestamp" Aeson..= toolExecTimestamp te,
        "duration_ms" Aeson..= (realToFrac (toolExecDuration te) * 1000 :: Double)
      ]

instance FromJSON ToolExecution where
  parseJSON = Aeson.withObject "ToolExecution" $ \v -> do
    name <- v Aeson..: "name"
    input <- v Aeson..: "input"
    outputObj <- v Aeson..: "output"
    output <- case AesonTypes.parseMaybe (Aeson..: "error") outputObj of
      Just err -> return $ Left err
      Nothing -> case AesonTypes.parseMaybe (Aeson..: "success") outputObj of
        Just out -> return $ Right out
        Nothing -> fail "Invalid tool execution output"
    timestamp <- v Aeson..: "timestamp"
    durationMs <- v Aeson..: "duration_ms"
    let duration = realToFrac (durationMs :: Double) / 1000
    return $ ToolExecution name input output timestamp duration

--------------------------------------------------------------------------------
-- Orchestration Types
--------------------------------------------------------------------------------

-- | Execution status for agents and workflows.
data ExecutionStatus
  = -- | Successful completion
    Success
  | -- | Completed with warnings
    PartialSuccess Text
  | -- | Failed with error message
    Failed Text
  deriving (Show, Eq, Generic)

instance ToJSON ExecutionStatus

instance FromJSON ExecutionStatus

-- | Error handling policy for sequential workflows.
data ErrorPolicy
  = -- | Stop workflow on first error
    StopOnError
  | -- | Log error and continue
    ContinueOnError
  deriving (Show, Eq, Generic)

instance ToJSON ErrorPolicy

instance FromJSON ErrorPolicy

-- | Strategy for aggregating results from parallel execution.
data AggregationStrategy
  = -- | Return all results
    CollectAll
  | -- | Use majority consensus
    MajorityVote
  | -- | Return first successful result
    FirstSuccess
  deriving (Show, Eq, Generic)

instance ToJSON AggregationStrategy

instance FromJSON AggregationStrategy

--------------------------------------------------------------------------------
-- Logging Types
--------------------------------------------------------------------------------

-- | Log severity levels.
data LogLevel
  = -- | Detailed debugging information
    DEBUG
  | -- | General informational messages
    INFO
  | -- | Warning messages
    WARN
  | -- | Error messages
    ERROR
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON LogLevel

instance FromJSON LogLevel

-- | Individual log entry with metadata.
data LogEntry = LogEntry
  { logLevel :: LogLevel,
    logTimestamp :: UTCTime,
    logAgentId :: Maybe AgentId,
    logToolName :: Maybe Text,
    logMessage :: Text,
    -- | Metadata as JSON object
    logMetadata :: AesonTypes.Value,
    -- | For nested agent calls
    logCallStack :: [AgentId]
  }
  deriving (Show, Eq, Generic)

instance ToJSON LogEntry

instance FromJSON LogEntry
