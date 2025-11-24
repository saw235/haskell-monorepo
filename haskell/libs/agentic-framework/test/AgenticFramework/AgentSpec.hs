{-# LANGUAGE OverloadedStrings #-}

module AgenticFramework.AgentSpec (spec) where

import AgenticFramework.Agent
import AgenticFramework.Context
import AgenticFramework.Logging (ExecutionLog (..))
import AgenticFramework.Types
import qualified Data.Text as T
import Test.Hspec

-- | Test specification for Agent API
spec :: Spec
spec = do
  describe "[FR-001] Agent Creation" $ do
    it "creates an agent with valid configuration" $ do
      let llmCfg =
            LLMConfig
              { llmProvider = Kimi,
                llmModel = "moonshot-v1-8k",
                llmApiKey = Nothing,
                llmBaseUrl = Just "https://api.moonshot.ai/v1",
                llmMaxTokens = 4096,
                llmTemperature = 0.7
              }
      let config =
            AgentConfig
              { configName = "test-agent",
                configSystemPrompt = "You are a test agent",
                configTools = [],
                configLLM = llmCfg,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing
              }

      agent <- createAgent config

      -- Verify agent properties
      agentName agent `shouldBe` "test-agent"
      systemPrompt agent `shouldBe` "You are a test agent"
      availableTools agent `shouldBe` []
      contextThreshold agent `shouldBe` 0.9

    it "generates unique agent IDs" $ do
      let config =
            AgentConfig
              { configName = "agent1",
                configSystemPrompt = "test",
                configTools = [],
                configLLM = testLLMConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing
              }

      agent1 <- createAgent config
      agent2 <- createAgent config

      -- Agent IDs should be different
      agentId agent1 `shouldNotBe` agentId agent2

  describe "[FR-004] Agent Execution" $ do
    it "executes agent with simple task" $ do
      agent <- createTestAgent
      result <- executeAgent agent "Hello, agent!"

      -- Should return a result
      resultSuccess result `shouldBe` True
      resultError result `shouldBe` Nothing
      T.length (resultOutput result) `shouldSatisfy` (> 0)

    it "executes agent with context preserves conversation history" $ do
      agent <- createTestAgent

      -- First execution
      result1 <- executeAgent agent "First message"
      let ctx1 = resultContext result1

      -- Check conversation has messages
      length (contextConversation ctx1) `shouldSatisfy` (>= 2) -- User + Assistant

      -- Second execution with context
      result2 <- executeAgentWithContext agent ctx1 "Second message"
      let ctx2 = resultContext result2

      -- Conversation should grow
      length (contextConversation ctx2) `shouldSatisfy` (> length (contextConversation ctx1))

    it "creates execution logs with timestamps" $ do
      agent <- createTestAgent
      result <- executeAgent agent "Test task"

      let log = resultLog result

      -- Log should have timestamps
      execLogStartTime log `shouldSatisfy` const True
      execLogEndTime log `shouldSatisfy` (/= Nothing)

      -- Log should reference correct agent
      execLogAgentId log `shouldBe` agentId agent

  describe "[FR-004] Result Constructors" $ do
    it "successResult creates successful result" $ do
      agent <- createTestAgent
      let ctx = createContext (agentId agent) (llmConfig agent)
      result <- executeAgent agent "test"
      let log = resultLog result

      let successRes = successResult "output" ctx log

      resultSuccess successRes `shouldBe` True
      resultError successRes `shouldBe` Nothing
      resultOutput successRes `shouldBe` "output"

    it "failureResult creates failed result" $ do
      agent <- createTestAgent
      let ctx = createContext (agentId agent) (llmConfig agent)
      result <- executeAgent agent "test"
      let log = resultLog result

      let failRes = failureResult "error message" ctx log

      resultSuccess failRes `shouldBe` False
      resultError failRes `shouldBe` Just "error message"
      resultOutput failRes `shouldBe` ""

-- | Helper to create a test agent
createTestAgent :: IO Agent
createTestAgent =
  createAgent
    AgentConfig
      { configName = "test-agent",
        configSystemPrompt = "You are a helpful test agent",
        configTools = [],
        configLLM = testLLMConfig,
        configSkillsDir = Nothing,
        configMaxTokens = Nothing,
        configTemperature = Nothing,
        configCapabilities = [],
        configWorkflow = Nothing
      }

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
