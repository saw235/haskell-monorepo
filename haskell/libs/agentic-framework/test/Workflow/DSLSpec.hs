{-# LANGUAGE OverloadedStrings #-}

module Workflow.DSLSpec (spec) where

import AgenticFramework.Types (LLMConfig (..), LLMProvider (..), Tool (..), ToolError (..), ToolInput (..), ToolOutput (..), ToolSchema (..))
import AgenticFramework.Workflow (runWorkflow)
import AgenticFramework.Workflow.DSL
import AgenticFramework.Workflow.Types
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

-- | Mock tool for testing
mockTool :: Tool
mockTool =
  Tool
    { toolName = "mock_tool",
      toolDescription = "A mock tool",
      toolSchema = ToolSchema (object []) (object []),
      toolExecute = \(ToolInput val) -> return $ Right $ ToolOutput val,
      toolTimeout = Nothing,
      toolRetryable = False
    }

-- | Helper to run workflow with mock context
runMockWorkflow :: Workflow a -> IO a
runMockWorkflow w = do
  historyRef <- newIORef []
  let ctx =
        AgentContext
          { ctxSystemPrompt = "system",
            ctxUserPrompt = "user",
            ctxTools = [mockTool],
            ctxCapabilities = [],
            ctxLLM = LLMConfig {llmProvider = Kimi, llmModel = "moonshot-v1-8k", llmApiKey = Nothing, llmBaseUrl = Just "https://api.moonshot.ai/v1", llmMaxTokens = 1000, llmTemperature = 0.7},
            ctxHistory = historyRef
          }
  let st =
        WorkflowState
          { stCurrentPhase = Init,
            stVariables = [],
            stStepCount = 0,
            stActiveCapabilities = []
          }
  runWorkflow w ctx st

spec :: Spec
spec = do
  describe "Workflow DSL" $ do
    describe "useTool" $ do
      it "executes a tool and returns its output" $ do
        let input = String "test_input"
        result <- runMockWorkflow $ useTool "mock_tool" input
        result `shouldBe` input

      it "throws error if tool not found" $ do
        runMockWorkflow (useTool "non_existent" Null) `shouldThrow` anyErrorCall

    describe "branch" $ do
      it "executes true branch when condition is true" $ do
        let w = branch (return True) (return "true") (return "false")
        result <- runMockWorkflow w
        result `shouldBe` "true"

      it "executes false branch when condition is false" $ do
        let w = branch (return False) (return "true") (return "false")
        result <- runMockWorkflow w
        result `shouldBe` "false"

    describe "getUserPrompt" $ do
      it "returns the user prompt from context" $ do
        result <- runMockWorkflow getUserPrompt
        result `shouldBe` "user"
