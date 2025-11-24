{-# LANGUAGE OverloadedStrings #-}

module AgenticFramework.ContextSpec (spec) where

import AgenticFramework.Context
import AgenticFramework.Types
import Data.Aeson (object)
import qualified Data.HashMap.Strict as HM
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Test.Hspec

-- | Test specification for Context API and token metrics
spec :: Spec
spec = do
  describe "[FR-002] Context Creation" $ do
    it "creates context with empty conversation" $ do
      let llmConfig =
            LLMConfig
              { llmProvider = Kimi,
                llmModel = "moonshot-v1-8k",
                llmApiKey = Nothing,
                llmBaseUrl = Just "https://api.moonshot.ai/v1",
                llmMaxTokens = 4096,
                llmTemperature = 0.7
              }

      let aid = AgentId testUUID
      let ctx = createContext aid llmConfig

      contextAgentId ctx `shouldBe` aid
      contextConversation ctx `shouldBe` []
      contextToolHistory ctx `shouldBe` []
      HM.null (contextLoadedSkills ctx) `shouldBe` True

    it "initializes token metrics correctly" $ do
      let llmConfig =
            LLMConfig
              { llmProvider = Kimi,
                llmModel = "moonshot-v1-8k",
                llmApiKey = Nothing,
                llmBaseUrl = Just "https://api.moonshot.ai/v1",
                llmMaxTokens = 8192,
                llmTemperature = 0.7
              }

      let ctx = createContext (AgentId testUUID) llmConfig
      let metrics = getTokenMetrics ctx

      currentTokenCount metrics `shouldBe` 0
      modelTokenLimit metrics `shouldBe` 8192
      percentageUsed metrics `shouldBe` 0.0
      summarizationTriggered metrics `shouldBe` False
      lastSummarization metrics `shouldBe` Nothing

  describe "[FR-002] Message Management" $ do
    it "addMessage appends message to conversation" $ do
      let ctx = createContext (AgentId testUUID) testLLMConfig
      timestamp <- getCurrentTime
      let msg = UserMessage "Test message" timestamp
      let ctx' = addMessage msg ctx

      length (contextConversation ctx') `shouldBe` 1
      head (contextConversation ctx') `shouldBe` msg

    it "addMessage preserves existing messages" $ do
      let ctx = createContext (AgentId testUUID) testLLMConfig
      timestamp <- getCurrentTime
      let msg1 = UserMessage "First" timestamp
      let msg2 = AssistantMessage "Second" timestamp
      let ctx' = addMessage msg1 ctx
      let ctx'' = addMessage msg2 ctx'

      length (contextConversation ctx'') `shouldBe` 2
      (contextConversation ctx'' !! 0) `shouldBe` msg1
      (contextConversation ctx'' !! 1) `shouldBe` msg2

  describe "[FR-002] Tool Execution History" $ do
    it "addToolExecution appends execution to history" $ do
      let ctx = createContext (AgentId testUUID) testLLMConfig
      timestamp <- getCurrentTime
      let toolExec =
            ToolExecution
              { toolExecName = "test-tool",
                toolExecInput = ToolInput $ object [],
                toolExecOutput = Right $ ToolOutput $ object [],
                toolExecTimestamp = timestamp,
                toolExecDuration = 0.1
              }
      let ctx' = addToolExecution toolExec ctx

      length (contextToolHistory ctx') `shouldBe` 1
      head (contextToolHistory ctx') `shouldBe` toolExec

  describe "[FR-002b] Token Metrics API" $ do
    it "getTokenMetrics returns current metrics" $ do
      let ctx = createContext (AgentId testUUID) testLLMConfig
      let metrics = getTokenMetrics ctx

      -- Verify metrics are returned (read-only access)
      metrics `shouldSatisfy` (\m -> currentTokenCount m >= 0)
      metrics `shouldSatisfy` (\m -> modelTokenLimit m > 0)
      metrics `shouldSatisfy` (\m -> percentageUsed m >= 0.0)

    it "getTokenMetrics is read-only" $ do
      -- This test verifies that getTokenMetrics doesn't modify context
      let ctx = createContext (AgentId testUUID) testLLMConfig
      let metrics1 = getTokenMetrics ctx
      let metrics2 = getTokenMetrics ctx

      -- Getting metrics multiple times should return same values
      metrics1 `shouldBe` metrics2

  describe "[FR-002,FR-022] Context State Management" $ do
    it "maintains call stack for circular detection" $ do
      let ctx = createContext (AgentId testUUID) testLLMConfig

      contextCallStack ctx `shouldBe` []
      contextMaxDepth ctx `shouldBe` 5

    it "maintains orchestration context" $ do
      let ctx = createContext (AgentId testUUID) testLLMConfig

      contextOrchestration ctx `shouldBe` Nothing

-- | Test helper: fake UUID for testing
testUUID :: UUID
testUUID = read "00000000-0000-0000-0000-000000000000"

-- | Test LLM configuration
testLLMConfig :: LLMConfig
testLLMConfig =
  LLMConfig
    { llmProvider = Kimi,
      llmModel = "moonshot-v1-8k",
      llmApiKey = Nothing,
      llmBaseUrl = Just "https://api.moonshot.ai/v1",
      llmMaxTokens = 4096,
      llmTemperature = 0.7
    }
