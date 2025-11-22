{-# LANGUAGE OverloadedStrings #-}

module Workflow.TypeSafetySpec (spec) where

import AgenticFramework.Types (LLMConfig (..), LLMProvider (..), Tool (..), ToolInput (..), ToolOutput (..), ToolSchema (..))
import AgenticFramework.Workflow
import AgenticFramework.Workflow.DSL
import AgenticFramework.Workflow.Types
import Data.Aeson (Value (..), object)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- | Mock tool that echoes its input
echoTool :: Tool
echoTool =
  Tool
    { toolName = "echo",
      toolDescription = "Echoes input",
      toolSchema = ToolSchema (object []) (object []),
      toolExecute = \(ToolInput val) -> return $ Right $ ToolOutput val,
      toolTimeout = Nothing,
      toolRetryable = False
    }

-- | Helper to create test context
mkTestContext :: Text -> [Tool] -> IO AgentContext
mkTestContext userPrompt tools = do
  historyRef <- newIORef []
  return
    AgentContext
      { ctxSystemPrompt = "test system",
        ctxUserPrompt = userPrompt,
        ctxTools = tools,
        ctxCapabilities = [],
        ctxLLM =
          LLMConfig
            { llmProvider = Kimi,
              llmModel = "moonshot-v1-8k",
              llmApiKey = Nothing,
              llmBaseUrl = Just "https://api.moonshot.ai/v1",
              llmMaxTokens = 1000,
              llmTemperature = 0.7
            },
        ctxHistory = historyRef
      }

-- | Initial workflow state
mkInitialState :: WorkflowState
mkInitialState =
  WorkflowState
    { stCurrentPhase = Init,
      stVariables = [],
      stStepCount = 0,
      stActiveCapabilities = []
    }

spec :: Spec
spec = do
  describe "Workflow Type Safety" $ do
    describe "Type Consistency" $ do
      it "preserves types through workflow steps" $ do
        ctx <- mkTestContext "test" [echoTool]
        let workflow = do
              prompt <- getUserPrompt
              return (T.length prompt)
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` 4

      it "handles Value types correctly in tool calls" $ do
        ctx <- mkTestContext "test" [echoTool]
        let workflow = do
              result <- useTool "echo" (String "hello")
              return result
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` String "hello"

    describe "Monad Laws" $ do
      it "satisfies left identity" $ do
        ctx <- mkTestContext "test" []
        let x = 5 :: Int
        let workflow1 :: Workflow Int
            workflow1 = return x >>= return
        let workflow2 :: Workflow Int
            workflow2 = return x
        result1 <- runWorkflow workflow1 ctx mkInitialState
        result2 <- runWorkflow workflow2 ctx mkInitialState
        result1 `shouldBe` result2

      it "satisfies right identity" $ do
        ctx <- mkTestContext "test" []
        let workflow :: Workflow Int
            workflow = do
              x <- return 42
              return x
        let workflow2 :: Workflow Int
            workflow2 = return 42
        result1 <- runWorkflow workflow ctx mkInitialState
        result2 <- runWorkflow workflow2 ctx mkInitialState
        result1 `shouldBe` result2

      it "satisfies associativity" $ do
        ctx <- mkTestContext "test" []
        let f :: Int -> Workflow Int
            f x = return (x + 1)
        let g :: Int -> Workflow Int
            g x = return (x * 2)
        let workflow1 = (return 5 >>= f) >>= g
        let workflow2 = return 5 >>= (\x -> f x >>= g)
        result1 <- runWorkflow workflow1 ctx mkInitialState
        result2 <- runWorkflow workflow2 ctx mkInitialState
        result1 `shouldBe` result2

    describe "Error Handling" $ do
      it "propagates errors from tool calls" $ do
        ctx <- mkTestContext "test" []
        let workflow = useTool "nonexistent" Null
        runWorkflow workflow ctx mkInitialState `shouldThrow` anyErrorCall

      it "handles missing tools gracefully" $ do
        ctx <- mkTestContext "test" [echoTool]
        let workflow = useTool "missing" (String "test")
        runWorkflow workflow ctx mkInitialState `shouldThrow` anyErrorCall

    describe "State Management" $ do
      it "maintains state across workflow steps" $ do
        ctx <- mkTestContext "test" []
        let workflow = do
              prompt1 <- getUserPrompt
              prompt2 <- getUserPrompt
              return (prompt1 == prompt2)
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` True

      it "allows multiple tool invocations" $ do
        ctx <- mkTestContext "test" [echoTool]
        let workflow = do
              r1 <- useTool "echo" (String "first")
              r2 <- useTool "echo" (String "second")
              return (r1, r2)
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` (String "first", String "second")
