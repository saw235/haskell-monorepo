{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Integration.ExecutionModeSpec
-- Description : Integration tests for execution mode selection (User Story 4)
-- Copyright   : (c) 2025
-- License     : MIT
--
-- Tests for FR-005: Execute agents with or without workflows
-- Tests verification of workflow execution mode vs traditional ReAct mode
module Integration.ExecutionModeSpec (spec) where

import AgenticFramework.Agent
import AgenticFramework.Context (AgentContext (..), createContext)
import AgenticFramework.Tool.LangChain (calculatorTool)
import AgenticFramework.Types
import AgenticFramework.Workflow (runWorkflow)
import AgenticFramework.Workflow.DSL (getUserPrompt, llmCall, useTool)
import AgenticFramework.Workflow.Execution (executeWorkflow)
import AgenticFramework.Workflow.Types
  ( AgentContext (..),
    Capability (..),
    Workflow (..),
    WorkflowError (..),
    WorkflowPhase (..),
    WorkflowState (..),
  )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.IORef (newIORef)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

-- | Integration tests for execution mode selection
spec :: LLMConfig -> Spec
spec llmConfig = do
  describe "[FR-005,SC-004] Workflow Execution Mode" $ do
    it "executes agent with workflow when workflow is present" $ do
      -- Create a simple workflow that processes user input
      let simpleWorkflow :: Workflow Text
          simpleWorkflow = do
            prompt <- getUserPrompt
            response <- llmCall $ "Echo: " <> prompt
            return response

      let agentConfig =
            AgentConfig
              { configName = "workflow-agent",
                configSystemPrompt = "You are a helpful assistant.",
                configTools = [],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing,
                configCapabilities = [],
                configWorkflow = Just simpleWorkflow
              }

      agent <- createAgent agentConfig
      result <- executeAgent agent "Hello from workflow test"

      -- Verify execution succeeded
      resultSuccess result `shouldBe` True

      -- The output should contain some response (workflow was executed)
      T.length (resultOutput result) `shouldSatisfy` (> 0)

    it "executes workflow with tool invocation" $ do
      -- Create a workflow that uses a tool
      let calculatorWorkflow :: Workflow Text
          calculatorWorkflow = do
            prompt <- getUserPrompt
            -- Use the calculator tool
            calcResult <- useTool "calculator" (object ["expression" .= ("2 + 2" :: Text)])
            return $ "Calculation result: " <> T.pack (show calcResult)

      let agentConfig =
            AgentConfig
              { configName = "calc-workflow-agent",
                configSystemPrompt = "You are a calculator assistant.",
                configTools = [calculatorTool],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing,
                configCapabilities = [],
                configWorkflow = Just calculatorWorkflow
              }

      agent <- createAgent agentConfig
      result <- executeAgent agent "Calculate something"

      -- Verify execution succeeded
      resultSuccess result `shouldBe` True

      -- Verify output contains calculation result
      T.pack "4" `shouldSatisfy` (`T.isInfixOf` resultOutput result)

  describe "[FR-005] Traditional ReAct Execution Mode" $ do
    it "executes agent without workflow using traditional mode" $ do
      let agentConfig =
            AgentConfig
              { configName = "traditional-agent",
                configSystemPrompt = "You are a helpful math assistant.",
                configTools = [calculatorTool],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing,
                configCapabilities = [],
                configWorkflow = Nothing -- No workflow = traditional mode
              }

      agent <- createAgent agentConfig
      result <- executeAgent agent "Calculate 10 * 5"

      -- Verify execution succeeded
      resultSuccess result `shouldBe` True

      -- Verify tool was used in traditional mode
      length (resultToolsUsed result) `shouldSatisfy` (> 0)

      -- Verify correct answer
      T.pack "50" `shouldSatisfy` (`T.isInfixOf` resultOutput result)

    it "traditional mode handles multi-turn conversation" $ do
      let agentConfig =
            AgentConfig
              { configName = "multi-turn-agent",
                configSystemPrompt = "You are a calculator assistant with memory.",
                configTools = [calculatorTool],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing,
                configCapabilities = [],
                configWorkflow = Nothing
              }

      agent <- createAgent agentConfig

      -- First turn
      result1 <- executeAgent agent "Calculate 3 + 3"
      resultSuccess result1 `shouldBe` True
      let ctx1 = resultContext result1

      -- Second turn with context
      result2 <- executeAgentWithContext agent ctx1 "Now calculate 6 * 2"
      resultSuccess result2 `shouldBe` True

      -- Verify context was preserved
      length (contextConversation (resultContext result2))
        `shouldSatisfy` (> length (contextConversation ctx1))

  describe "[FR-005] Execution Mode Selection" $ do
    it "selects workflow mode when workflow is present" $ do
      let hasWorkflow =
            AgentConfig
              { configName = "with-workflow",
                configSystemPrompt = "Test",
                configTools = [],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing,
                configCapabilities = [],
                configWorkflow = Just (return "workflow result" :: Workflow Text)
              }

      agent <- createAgent hasWorkflow

      -- Agent should have a workflow
      isJust (agentWorkflow agent) `shouldBe` True

    it "selects traditional mode when no workflow is present" $ do
      let noWorkflow =
            AgentConfig
              { configName = "without-workflow",
                configSystemPrompt = "Test",
                configTools = [calculatorTool],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing,
                configCapabilities = [],
                configWorkflow = Nothing
              }

      agent <- createAgent noWorkflow

      -- Agent should not have a workflow
      isNothing (agentWorkflow agent) `shouldBe` True

    it "same agent type supports both execution modes" $ do
      -- Create an agent without workflow first
      let configWithoutWorkflow =
            AgentConfig
              { configName = "dual-mode-agent",
                configSystemPrompt = "You are a versatile assistant.",
                configTools = [calculatorTool],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing,
                configCapabilities = [],
                configWorkflow = Nothing
              }

      agent1 <- createAgent configWithoutWorkflow
      result1 <- executeAgent agent1 "Calculate 7 + 7"
      resultSuccess result1 `shouldBe` True

      -- Create same agent type but with workflow
      let simpleWorkflow :: Workflow Text
          simpleWorkflow = do
            prompt <- getUserPrompt
            return $ "Processed: " <> prompt

      let configWithWorkflow =
            configWithoutWorkflow
              { configWorkflow = Just simpleWorkflow
              }

      agent2 <- createAgent configWithWorkflow
      result2 <- executeAgent agent2 "Test input"
      resultSuccess result2 `shouldBe` True

      -- Both agents should have executed successfully
      T.length (resultOutput result1) `shouldSatisfy` (> 0)
      T.length (resultOutput result2) `shouldSatisfy` (> 0)
