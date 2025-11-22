{-# LANGUAGE OverloadedStrings #-}

module Workflow.CompositionSpec (spec) where

import AgenticFramework.Workflow
import AgenticFramework.Workflow.DSL
import AgenticFramework.Workflow.Types
import AgenticFramework.Types (LLMConfig (..), LLMProvider (..), Tool (..), ToolSchema (..), ToolInput (..), ToolOutput (..))
import Data.Aeson (Value (..), object)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

-- | Mock tools for composition testing
addTool :: Tool
addTool =
  Tool
    { toolName = "add",
      toolDescription = "Adds two numbers",
      toolSchema = ToolSchema (object []) (object []),
      toolExecute = \(ToolInput (Object _)) -> return $ Right $ ToolOutput $ Number 42,
      toolTimeout = Nothing,
      toolRetryable = False
    }

multiplyTool :: Tool
multiplyTool =
  Tool
    { toolName = "multiply",
      toolDescription = "Multiplies two numbers",
      toolSchema = ToolSchema (object []) (object []),
      toolExecute = \(ToolInput (Number n)) -> return $ Right $ ToolOutput $ Number (n * 2),
      toolTimeout = Nothing,
      toolRetryable = False
    }

-- | Helper to create test context
mkTestContext :: [Tool] -> IO AgentContext
mkTestContext tools = do
  historyRef <- newIORef []
  return
    AgentContext
      { ctxSystemPrompt = "test system",
        ctxUserPrompt = "test user prompt",
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
  describe "Workflow Composition" $ do
    describe "Sequential Composition" $ do
      it "chains workflows with >>=" $ do
        ctx <- mkTestContext []
        let workflow = return 5 >>= \x -> return (x * 2)
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` 10

      it "composes multiple steps" $ do
        ctx <- mkTestContext []
        let workflow = do
              x <- return 3
              y <- return 4
              return (x + y)
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` 7

      it "chains tool calls sequentially" $ do
        ctx <- mkTestContext [addTool, multiplyTool]
        let workflow = do
              result1 <- useTool "add" (object [])
              result2 <- useTool "multiply" result1
              return result2
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` Number 84

    describe "Data Flow Between Steps" $ do
      it "passes data from one step to the next" $ do
        ctx <- mkTestContext []
        let workflow = do
              prompt <- getUserPrompt
              let upperPrompt = T.toUpper prompt
              return upperPrompt
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` "TEST USER PROMPT"

      it "transforms data through multiple steps" $ do
        ctx <- mkTestContext []
        let workflow = do
              let initial = "hello"
              let step1 = T.toUpper initial
              let step2 = T.append step1 " WORLD"
              return step2
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` "HELLO WORLD"

      it "maintains data integrity across steps" $ do
        ctx <- mkTestContext []
        let workflow = do
              let numbers = [1, 2, 3, 4, 5] :: [Int]
              let doubled = map (* 2) numbers
              let summed = sum doubled
              return summed
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` 30

    describe "Branching and Control Flow" $ do
      it "executes conditional branches" $ do
        ctx <- mkTestContext []
        let workflow = branch (return True) (return "yes") (return "no")
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` "yes"

      it "allows nested branching" $ do
        ctx <- mkTestContext []
        let workflow =
              branch
                (return True)
                (branch (return False) (return "A") (return "B"))
                (return "C")
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` "B"

      it "branches based on computed conditions" $ do
        ctx <- mkTestContext []
        let workflow = do
              x <- return 10
              branch (return (x > 5)) (return "large") (return "small")
        result <- runWorkflow workflow ctx mkInitialState
        result `shouldBe` "large"

    describe "Workflow Nesting" $ do
      it "calls sub-workflows" $ do
        ctx <- mkTestContext []
        let subWorkflow :: Workflow Int
            subWorkflow = return 42
        let mainWorkflow = do
              result <- subWorkflow
              return (result + 1)
        result <- runWorkflow mainWorkflow ctx mkInitialState
        result `shouldBe` 43

      it "composes sub-workflows with data flow" $ do
        ctx <- mkTestContext []
        let processText :: Text -> Workflow Text
            processText t = return (T.toUpper t)
        let mainWorkflow = do
              prompt <- getUserPrompt
              processed <- processText prompt
              return processed
        result <- runWorkflow mainWorkflow ctx mkInitialState
        result `shouldBe` "TEST USER PROMPT"

      it "allows multiple levels of nesting" $ do
        ctx <- mkTestContext []
        let level3 :: Workflow Int
            level3 = return 1
        let level2 :: Workflow Int
            level2 = do
              x <- level3
              return (x + 1)
        let level1 :: Workflow Int
            level1 = do
              y <- level2
              return (y + 1)
        result <- runWorkflow level1 ctx mkInitialState
        result `shouldBe` 3

    describe "Error Propagation" $ do
      it "stops execution on error" $ do
        ctx <- mkTestContext []
        let workflow = do
              _ <- useTool "nonexistent" Null
              return "should not reach"
        runWorkflow workflow ctx mkInitialState `shouldThrow` anyErrorCall

      it "propagates errors through composition" $ do
        ctx <- mkTestContext []
        let subWorkflow = useTool "missing" Null
        let mainWorkflow = do
              _ <- subWorkflow
              return "unreachable"
        runWorkflow mainWorkflow ctx mkInitialState `shouldThrow` anyErrorCall
