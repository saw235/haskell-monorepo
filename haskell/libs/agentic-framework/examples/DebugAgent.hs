{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : DebugAgent
-- Description : Example showing both workflow and traditional execution modes
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This example demonstrates how agents can execute in either workflow mode
-- or traditional ReAct mode, as specified in FR-005 (User Story 4).
--
-- = Usage
--
-- @
-- bazel run //haskell/libs/agentic-framework/examples:debug-agent
-- @
module Main where

import AgenticFramework.Agent
import AgenticFramework.Tool.LangChain (calculatorTool)
import AgenticFramework.Types
import AgenticFramework.Workflow.DSL (getUserPrompt, llmCall, useTool)
import AgenticFramework.Workflow.Types (Capability (..), Workflow)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)

-- | Main entry point demonstrating both execution modes
main :: IO ()
main = do
  TIO.putStrLn "=== Debug Agent Example (FR-005) ==="
  TIO.putStrLn "Demonstrating workflow vs traditional execution modes\n"

  -- Create LLM configuration
  llmConfig <- createLLMConfig

  -- Example 1: Traditional mode (no workflow)
  TIO.putStrLn "--- Mode 1: Traditional ReAct Execution ---"
  runTraditionalMode llmConfig

  -- Example 2: Workflow mode
  TIO.putStrLn "\n--- Mode 2: Workflow-Based Execution ---"
  runWorkflowMode llmConfig

  -- Example 3: Same agent can be used in both modes
  TIO.putStrLn "\n--- Mode 3: Dual-Mode Agent ---"
  runDualModeDemo llmConfig

  TIO.putStrLn "\n=== Example Complete ==="

-- | Run agent in traditional ReAct mode (no workflow)
runTraditionalMode :: LLMConfig -> IO ()
runTraditionalMode llmConfig = do
  let config =
        AgentConfig
          { configName = "traditional-agent",
            configSystemPrompt = "You are a helpful calculator assistant.",
            configTools = [calculatorTool],
            configLLM = llmConfig,
            configSkillsDir = Nothing,
            configMaxTokens = Nothing,
            configTemperature = Nothing,
            configCapabilities = [],
            configWorkflow = Nothing -- No workflow = traditional mode
          }

  agent <- createAgent config
  TIO.putStrLn $ "Created agent: " <> agentName agent
  TIO.putStrLn "Workflow present: No (traditional mode)"

  result <- executeAgent agent "Calculate 42 * 7"

  TIO.putStrLn $ "Success: " <> T.pack (show (resultSuccess result))
  TIO.putStrLn $ "Output: " <> resultOutput result
  TIO.putStrLn $ "Tools used: " <> T.pack (show (length (resultToolsUsed result)))

-- | Run agent in workflow mode
runWorkflowMode :: LLMConfig -> IO ()
runWorkflowMode llmConfig = do
  -- Define a debug workflow that shows its execution steps
  let debugWorkflow :: Workflow Text
      debugWorkflow = do
        prompt <- getUserPrompt
        -- First, acknowledge the input
        response1 <- llmCall $ "Received: " <> prompt <> ". Processing..."
        -- Then provide a response
        response2 <- llmCall $ "Task completed for input: " <> prompt
        return $ "Debug log:\n1. " <> response1 <> "\n2. " <> response2

  let config =
        AgentConfig
          { configName = "workflow-agent",
            configSystemPrompt = "You are a debugging assistant.",
            configTools = [],
            configLLM = llmConfig,
            configSkillsDir = Nothing,
            configMaxTokens = Nothing,
            configTemperature = Nothing,
            configCapabilities = [],
            configWorkflow = Just debugWorkflow -- Workflow mode
          }

  agent <- createAgent config
  TIO.putStrLn $ "Created agent: " <> agentName agent
  TIO.putStrLn "Workflow present: Yes (workflow mode)"

  result <- executeAgent agent "Debug this task"

  TIO.putStrLn $ "Success: " <> T.pack (show (resultSuccess result))
  TIO.putStrLn $ "Output: " <> resultOutput result

-- | Demonstrate that the same agent type can work in both modes
runDualModeDemo :: LLMConfig -> IO ()
runDualModeDemo llmConfig = do
  -- First, create and run without workflow
  let configNoWorkflow =
        AgentConfig
          { configName = "dual-mode-demo",
            configSystemPrompt = "You are a versatile assistant.",
            configTools = [calculatorTool],
            configLLM = llmConfig,
            configSkillsDir = Nothing,
            configMaxTokens = Nothing,
            configTemperature = Nothing,
            configCapabilities = [],
            configWorkflow = Nothing
          }

  agentTraditional <- createAgent configNoWorkflow
  TIO.putStrLn $ "Agent 1 (traditional): " <> agentName agentTraditional
  result1 <- executeAgent agentTraditional "Calculate 10 + 20"
  TIO.putStrLn $ "Result 1: " <> resultOutput result1

  -- Then, create same agent type with workflow
  let simpleWorkflow :: Workflow Text
      simpleWorkflow = do
        prompt <- getUserPrompt
        return $ "Workflow processed: " <> prompt

  let configWithWorkflow =
        configNoWorkflow
          { configWorkflow = Just simpleWorkflow
          }

  agentWorkflow <- createAgent configWithWorkflow
  TIO.putStrLn $ "\nAgent 2 (workflow): " <> agentName agentWorkflow
  result2 <- executeAgent agentWorkflow "Process this input"
  TIO.putStrLn $ "Result 2: " <> resultOutput result2

  TIO.putStrLn "\nBoth agents executed successfully with different modes!"

-- | Create LLM configuration from environment or defaults
createLLMConfig :: IO LLMConfig
createLLMConfig = do
  apiKey <- lookupEnv "KIMI_API_KEY"
  return
    LLMConfig
      { llmProvider = Kimi,
        llmModel = "moonshot-v1-8k",
        llmApiKey = T.pack <$> apiKey,
        llmBaseUrl = Just "https://api.moonshot.ai/v1",
        llmMaxTokens = 4096,
        llmTemperature = 0.7
      }
