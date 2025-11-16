{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : AgenticFramework.Agent
Description : Core agent functionality for autonomous reasoning and tool execution
Copyright   : (c) 2025
License     : MIT

This module provides the core Agent functionality, including agent creation,
configuration, and execution. Agents are autonomous reasoning entities that
can use tools to accomplish tasks.

= Usage

@
import AgenticFramework.Agent
import AgenticFramework.Tool

main :: IO ()
main = do
  let agentConfig = AgentConfig
        { configName = "assistant"
        , configSystemPrompt = "You are a helpful assistant"
        , configTools = [calculatorTool]
        , configLLM = llmConfig
        , configSkillsDir = Nothing
        , configMaxTokens = Nothing
        , configTemperature = Nothing
        }

  agent <- createAgent agentConfig
  result <- executeAgent agent "What is 2 + 2?"
  print (resultOutput result)
@
-}

module AgenticFramework.Agent
  ( -- * Agent Types
    Agent (..)
  , AgentConfig (..)
  , AgentResult (..)

    -- * Agent Creation and Execution
  , createAgent
  , executeAgent
  , executeAgentWithContext

    -- * Result Constructors
  , successResult
  , failureResult

  ) where

import AgenticFramework.Types
import AgenticFramework.Context (AgentContext(..), createContext, addMessage)
import AgenticFramework.Logging (ExecutionLog(..))
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------
-- Agent Data Types
--------------------------------------------------------------------------------

-- | An autonomous agent that can reason and use tools.
data Agent = Agent
  { agentId :: AgentId
  , agentName :: Text
  , systemPrompt :: Text
  , availableTools :: [Tool]
  , llmConfig :: LLMConfig
  , skillsDirectory :: Maybe FilePath
  , maxTokens :: Maybe Int
  , temperature :: Maybe Double
  , contextThreshold :: Double  -- Default 0.9 (90%)
  } deriving (Show, Generic)

-- | Configuration for creating a new agent.
data AgentConfig = AgentConfig
  { configName :: Text
  , configSystemPrompt :: Text
  , configTools :: [Tool]
  , configLLM :: LLMConfig
  , configSkillsDir :: Maybe FilePath
  , configMaxTokens :: Maybe Int
  , configTemperature :: Maybe Double
  } deriving (Show, Generic)

-- | Result from agent execution.
data AgentResult = AgentResult
  { resultOutput :: Text
  , resultContext :: AgentContext
  , resultLog :: ExecutionLog
  , resultToolsUsed :: [ToolExecution]
  , resultSuccess :: Bool
  , resultError :: Maybe Text
  } deriving (Show, Generic)

--------------------------------------------------------------------------------
-- Agent Creation and Execution
--------------------------------------------------------------------------------

-- | Create a new agent from configuration.
--   Generates a unique agent ID and initializes the agent.
createAgent :: AgentConfig -> IO Agent
createAgent config = do
  -- Generate unique agent ID
  agentUUID <- generateUUID
  let aid = AgentId agentUUID

  return Agent
    { agentId = aid
    , agentName = configName config
    , systemPrompt = configSystemPrompt config
    , availableTools = configTools config
    , llmConfig = configLLM config
    , skillsDirectory = configSkillsDir config
    , maxTokens = configMaxTokens config
    , temperature = configTemperature config
    , contextThreshold = 0.9  -- 90% default threshold
    }
  where
    -- Generate a random UUID v4
    generateUUID :: IO UUID
    generateUUID = UUID4.nextRandom

-- | Execute an agent with a new task.
--   Creates a fresh context and runs the agent until completion or error.
executeAgent :: Agent -> Text -> IO AgentResult
executeAgent agent input = do
  -- Create fresh context
  let ctx = createContext (agentId agent) (llmConfig agent)

  -- Execute with context
  executeAgentWithContext agent ctx input

-- | Execute an agent with existing context (for multi-turn conversations).
--   Preserves conversation history and token metrics from previous execution.
executeAgentWithContext :: Agent -> AgentContext -> Text -> IO AgentResult
executeAgentWithContext agent ctx input = do
  -- Create initial user message
  timestamp <- getCurrentTime
  let userMsg = UserMessage
        { messageContent = input
        , messageTimestamp = timestamp
        }

  -- Add user message to context
  let ctx' = addMessage userMsg ctx

  -- Create execution log
  startTime <- getCurrentTime
  logId <- UUID4.nextRandom
  let execLog = ExecutionLog
        { execLogId = logId
        , execLogAgentId = agentId agent
        , execLogEntries = Seq.empty
        , execLogStartTime = startTime
        , execLogEndTime = Nothing
        }

  -- For MVP, we'll create a simple stub response
  -- TODO: Implement full ReAct loop with langchain-hs LLM calls
  -- TODO: Implement tool selection and execution
  -- TODO: Implement iterative reasoning loop

  assistantTimestamp <- getCurrentTime
  let response = "This is a stub response. Full agent execution with langchain-hs will be implemented in T020-T021."
  let assistantMsg = AssistantMessage
        { messageContent = response
        , messageTimestamp = assistantTimestamp
        }

  let finalCtx = addMessage assistantMsg ctx'

  endTime <- getCurrentTime
  let finalLog = execLog { execLogEndTime = Just endTime }

  return $ successResult response finalCtx finalLog

--------------------------------------------------------------------------------
-- Result Constructors
--------------------------------------------------------------------------------

-- | Create a successful agent result.
successResult :: Text -> AgentContext -> ExecutionLog -> AgentResult
successResult output ctx log = AgentResult
  { resultOutput = output
  , resultContext = ctx
  , resultLog = log
  , resultToolsUsed = contextToolHistory ctx
  , resultSuccess = True
  , resultError = Nothing
  }

-- | Create a failed agent result.
failureResult :: Text -> AgentContext -> ExecutionLog -> AgentResult
failureResult err ctx log = AgentResult
  { resultOutput = ""
  , resultContext = ctx
  , resultLog = log
  , resultToolsUsed = contextToolHistory ctx
  , resultSuccess = False
  , resultError = Just err
  }
