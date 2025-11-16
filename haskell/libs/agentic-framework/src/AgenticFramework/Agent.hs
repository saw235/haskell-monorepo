{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Agent
-- Description : Core agent functionality for autonomous reasoning and tool execution
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides the core Agent functionality, including agent creation,
-- configuration, and execution. Agents are autonomous reasoning entities that
-- can use tools to accomplish tasks.
--
-- = Usage
--
-- @
-- import AgenticFramework.Agent
-- import AgenticFramework.Tool
--
-- main :: IO ()
-- main = do
--   let agentConfig = AgentConfig
--         { configName = "assistant"
--         , configSystemPrompt = "You are a helpful assistant"
--         , configTools = [calculatorTool]
--         , configLLM = llmConfig
--         , configSkillsDir = Nothing
--         , configMaxTokens = Nothing
--         , configTemperature = Nothing
--         }
--
--   agent <- createAgent agentConfig
--   result <- executeAgent agent "What is 2 + 2?"
--   print (resultOutput result)
-- @
module AgenticFramework.Agent
  ( -- * Agent Types
    Agent (..),
    AgentConfig (..),
    AgentResult (..),

    -- * Agent Creation and Execution
    createAgent,
    executeAgent,
    executeAgentWithContext,

    -- * Result Constructors
    successResult,
    failureResult,
  )
where

import AgenticFramework.Context (AgentContext (..), addMessage, addToolExecution, createContext, getTokenMetrics)
import qualified AgenticFramework.Context.Summarization as Sum
import AgenticFramework.LLM.Kimi (KimiLLM, createKimiLLM, defaultKimiParams)
import AgenticFramework.LLM.Ollama (OllamaLLM, createOllamaLLM, defaultOllamaParams)
import AgenticFramework.Logging (ExecutionLog (..), logAgentReasoning)
import AgenticFramework.Tool (executeTool)
import AgenticFramework.Types
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import GHC.Generics (Generic)
import qualified Langchain.LLM.Core as LLM

--------------------------------------------------------------------------------
-- Agent Data Types
--------------------------------------------------------------------------------

-- | An autonomous agent that can reason and use tools.
data Agent = Agent
  { agentId :: AgentId,
    agentName :: Text,
    systemPrompt :: Text,
    availableTools :: [Tool],
    llmConfig :: LLMConfig,
    skillsDirectory :: Maybe FilePath,
    maxTokens :: Maybe Int,
    temperature :: Maybe Double,
    contextThreshold :: Double -- Default 0.9 (90%)
  }
  deriving (Show, Generic)

-- | Configuration for creating a new agent.
data AgentConfig = AgentConfig
  { configName :: Text,
    configSystemPrompt :: Text,
    configTools :: [Tool],
    configLLM :: LLMConfig,
    configSkillsDir :: Maybe FilePath,
    configMaxTokens :: Maybe Int,
    configTemperature :: Maybe Double
  }
  deriving (Show, Generic)

-- | Result from agent execution.
data AgentResult = AgentResult
  { resultOutput :: Text,
    resultContext :: AgentContext,
    resultLog :: ExecutionLog,
    resultToolsUsed :: [ToolExecution],
    resultSuccess :: Bool,
    resultError :: Maybe Text
  }
  deriving (Show, Generic)

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

  return
    Agent
      { agentId = aid,
        agentName = configName config,
        systemPrompt = configSystemPrompt config,
        availableTools = configTools config,
        llmConfig = configLLM config,
        skillsDirectory = configSkillsDir config,
        maxTokens = configMaxTokens config,
        temperature = configTemperature config,
        contextThreshold = 0.9 -- 90% default threshold
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
--   Automatically triggers context summarization if token usage exceeds 90% threshold.
executeAgentWithContext :: Agent -> AgentContext -> Text -> IO AgentResult
executeAgentWithContext agent ctx input = do
  -- Check if we need to summarize context before proceeding (FR-042, T037)
  ctx' <-
    if Sum.shouldTriggerSummarization Sum.defaultSummarizationConfig ctx
      then Sum.summarizeContext (llmConfig agent) Sum.defaultSummarizationConfig ctx
      else return ctx

  -- Create initial user message
  timestamp <- getCurrentTime
  let userMsg =
        UserMessage
          { messageContent = input,
            messageTimestamp = timestamp
          }

  -- Add user message to context
  let ctxWithUserMsg = addMessage userMsg ctx'

  -- Check if we need to summarize after adding user message (FR-042)
  -- This catches cases where the user message itself pushes us over the threshold
  ctx'' <-
    if Sum.shouldTriggerSummarization Sum.defaultSummarizationConfig ctxWithUserMsg
      then Sum.summarizeContext (llmConfig agent) Sum.defaultSummarizationConfig ctxWithUserMsg
      else return ctxWithUserMsg

  -- Create execution log
  startTime <- getCurrentTime
  logId <- UUID4.nextRandom
  let execLog =
        ExecutionLog
          { execLogId = logId,
            execLogAgentId = agentId agent,
            execLogEntries = Seq.empty,
            execLogStartTime = startTime,
            execLogEndTime = Nothing
          }

  -- Check if this is a simple tool request that we can handle directly
  -- For now, implement basic pattern matching for calculator tool
  case tryDirectToolExecution agent input of
    Just (toolName, toolInput) -> do
      -- Direct tool execution path
      case findTool (availableTools agent) toolName of
        Nothing -> do
          endTime <- getCurrentTime
          let finalLog = execLog {execLogEndTime = Just endTime}
          return $ failureResult ("Tool not found: " <> toolName) ctx'' finalLog
        Just tool -> do
          -- Execute the tool
          toolResult <- executeTool tool toolInput

          case toolResult of
            Left toolErr -> do
              endTime <- getCurrentTime
              let finalLog = execLog {execLogEndTime = Just endTime}
              let errMsg = case toolErr of
                    ToolExecutionError msg -> "Tool execution error: " <> msg
                    ToolTimeoutError -> "Tool execution timed out"
              return $ failureResult errMsg ctx'' finalLog
            Right toolOutputResult -> do
              -- Record tool execution
              toolExecTime <- getCurrentTime
              let toolExec =
                    ToolExecution
                      { toolExecName = toolName,
                        toolExecInput = toolInput,
                        toolExecOutput = Right toolOutputResult,
                        toolExecTimestamp = toolExecTime,
                        toolExecDuration = 0 -- TODO: track actual duration
                      }

              let ctx''' = addToolExecution toolExec ctx''

              -- Extract output value
              let (ToolOutput output) = toolOutputResult

              -- Format response with tool result
              let response = formatToolResponse toolName output

              assistantTimestamp <- getCurrentTime
              let assistantMsg =
                    AssistantMessage
                      { messageContent = response,
                        messageTimestamp = assistantTimestamp
                      }

              let ctxAfterResponse = addMessage assistantMsg ctx'''

              -- Check if we need to summarize after adding the response (FR-042)
              finalCtx <-
                if Sum.shouldTriggerSummarization Sum.defaultSummarizationConfig ctxAfterResponse
                  then Sum.summarizeContext (llmConfig agent) Sum.defaultSummarizationConfig ctxAfterResponse
                  else return ctxAfterResponse

              endTime <- getCurrentTime
              let finalLog = execLog {execLogEndTime = Just endTime}

              return $ successResult response finalCtx finalLog
    Nothing -> do
      -- Fall back to LLM-only execution (no tool use)
      -- TODO: Implement full ReAct loop with LLM-driven tool selection
      let fullPrompt = systemPrompt agent <> "\n\nUser: " <> input

      llmResult <- case llmProvider (llmConfig agent) of
        Ollama -> do
          let ollamaLLM = createOllamaLLM (llmConfig agent)
          LLM.generate ollamaLLM fullPrompt (Just defaultOllamaParams)
        Kimi -> do
          case createKimiLLM (llmConfig agent) of
            Nothing -> return $ Left "Kimi API key not provided in LLMConfig"
            Just kimiLLM -> LLM.generate kimiLLM fullPrompt (Just defaultKimiParams)
        _ -> return $ Left "LLM provider not yet implemented"

      case llmResult of
        Left err -> do
          endTime <- getCurrentTime
          let finalLog = execLog {execLogEndTime = Just endTime}
          return $ failureResult (T.pack err) ctx'' finalLog
        Right response -> do
          assistantTimestamp <- getCurrentTime
          let assistantMsg =
                AssistantMessage
                  { messageContent = response,
                    messageTimestamp = assistantTimestamp
                  }

          let ctxAfterResponse = addMessage assistantMsg ctx''

          -- Check if we need to summarize after adding the response (FR-042)
          finalCtx <-
            if Sum.shouldTriggerSummarization Sum.defaultSummarizationConfig ctxAfterResponse
              then Sum.summarizeContext (llmConfig agent) Sum.defaultSummarizationConfig ctxAfterResponse
              else return ctxAfterResponse

          endTime <- getCurrentTime
          let finalLog = execLog {execLogEndTime = Just endTime}

          return $ successResult response finalCtx finalLog

--------------------------------------------------------------------------------
-- Result Constructors
--------------------------------------------------------------------------------

-- | Create a successful agent result.
successResult :: Text -> AgentContext -> ExecutionLog -> AgentResult
successResult output ctx log =
  AgentResult
    { resultOutput = output,
      resultContext = ctx,
      resultLog = log,
      resultToolsUsed = contextToolHistory ctx,
      resultSuccess = True,
      resultError = Nothing
    }

-- | Create a failed agent result.
failureResult :: Text -> AgentContext -> ExecutionLog -> AgentResult
failureResult err ctx log =
  AgentResult
    { resultOutput = "",
      resultContext = ctx,
      resultLog = log,
      resultToolsUsed = contextToolHistory ctx,
      resultSuccess = False,
      resultError = Just err
    }

--------------------------------------------------------------------------------
-- Helper Functions for Tool Execution
--------------------------------------------------------------------------------

-- | Try to parse user input as a direct tool invocation
--   Returns (toolName, toolInput) if successful
tryDirectToolExecution :: Agent -> Text -> Maybe (Text, ToolInput)
tryDirectToolExecution agent input =
  -- Pattern match for calculator tool: "Calculate X * Y" or "Calculate X + Y" etc.
  if "calculate" `T.isInfixOf` T.toLower input
    then
      let expr = extractExpression input
       in if T.null expr
            then Nothing
            else Just ("calculator", ToolInput $ object ["expression" .= expr])
    else -- Pattern match for file reader: "Read the file at X" or "Read X"

      if "read" `T.isInfixOf` T.toLower input && ("file" `T.isInfixOf` T.toLower input || "path" `T.isInfixOf` T.toLower input)
        then case extractFilePath input of
          Nothing -> Nothing
          Just path -> Just ("read_file", ToolInput $ object ["path" .= path])
        else Nothing

-- | Extract mathematical expression from text
--   "Calculate 15 * 23" -> "15 * 23"
extractExpression :: Text -> Text
extractExpression input =
  let lower = T.toLower input
      -- Find "calculate" and take everything after it
      afterCalculate = case T.breakOn "calculate" lower of
        (_, rest) -> T.drop (T.length "calculate") rest
   in T.strip afterCalculate

-- | Extract file path from text
--   "Read the file at /path/to/file" -> Just "/path/to/file"
extractFilePath :: Text -> Maybe Text
extractFilePath input =
  let lower = T.toLower input
      -- Look for common patterns
      patterns = ["at path:", "at:", "path:", "file:"]
      tryPattern pattern = case T.breakOn pattern lower of
        (_, "") -> Nothing
        (_, rest) -> Just $ T.strip $ T.drop (T.length pattern) rest
   in List.find (not . T.null . T.strip) $ map (maybe "" id . tryPattern) patterns

-- | Find a tool by name in the agent's available tools
findTool :: [Tool] -> Text -> Maybe Tool
findTool tools name = List.find (\t -> toolName t == name) tools

-- | Format tool response for the user
formatToolResponse :: Text -> Value -> Text
formatToolResponse "calculator" output =
  case KM.lookup "result" (getObject output) of
    Just (Aeson.Number result) -> "The result is: " <> T.pack (show result)
    _ -> "Calculation completed: " <> T.pack (show output)
formatToolResponse "read_file" output =
  case KM.lookup "content" (getObject output) of
    Just (Aeson.String content) -> "File contents:\n" <> content
    _ -> "File read completed: " <> T.pack (show output)
formatToolResponse toolName output =
  "Tool " <> toolName <> " executed successfully. Result: " <> T.pack (show output)

-- | Extract object from Aeson Value
getObject :: Value -> KM.KeyMap Value
getObject (Aeson.Object obj) = obj
getObject _ = KM.empty
