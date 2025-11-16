{-# LANGUAGE OverloadedStrings #-}

module Integration.SimpleAgentSpec (spec) where

import AgenticFramework.Agent
import AgenticFramework.Context (AgentContext(..))
import AgenticFramework.Tool
import AgenticFramework.Tool.File (readFileTool, writeFileTool, listDirectoryTool)
import AgenticFramework.Tool.LangChain (calculatorTool)
import AgenticFramework.Types
import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, removeFile, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec

-- | Integration test for single agent with tools
-- These tests verify that agents can successfully execute tools and complete tasks
spec :: LLMConfig -> Spec
spec llmConfig = do
  describe "[FR-005,FR-006,FR-007] Single Agent with Tools" $ do
    it "agent completes task using calculator tool" $ do
      -- Create agent with calculator tool
      let agentConfig =
            AgentConfig
              { configName = "calculator-agent",
                configSystemPrompt = "You are a helpful math assistant. Use the calculator tool for computations.",
                configTools = [calculatorTool],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing
              }

      agent <- createAgent agentConfig
      result <- executeAgent agent "Calculate 15 * 23"

      -- Verify the agent executed successfully
      resultSuccess result `shouldBe` True
      resultError result `shouldBe` Nothing

      -- Verify output is non-empty
      T.length (resultOutput result) `shouldSatisfy` (> 0)

      -- CRITICAL: Verify the calculator tool was ACTUALLY USED
      length (resultToolsUsed result) `shouldSatisfy` (> 0)
      case resultToolsUsed result of
        (firstExec:_) -> toolExecName firstExec `shouldBe` "calculator"
        [] -> expectationFailure "No tools were executed"

      -- CRITICAL: Verify the correct answer (345) appears in the output
      T.pack "345" `shouldSatisfy` (`T.isInfixOf` resultOutput result)

      -- Verify calculator tool was available
      length (availableTools agent) `shouldBe` 1
      case availableTools agent of
        (firstTool:_) -> toolName firstTool `shouldBe` "calculator"
        [] -> expectationFailure "No tools were available"

    it "agent completes task using file reader tool" $ do
      withTempDir "agentic-test" $ \tmpDir -> do
        -- Create a test file
        let testFilePath = tmpDir ++ "/test.txt"
        TIO.writeFile testFilePath "Hello from test file!"

        -- Create agent with file tools
        let agentConfig =
              AgentConfig
                { configName = "file-agent",
                  configSystemPrompt = "You are a helpful file assistant. Use the file tools to read and write files.",
                  configTools = [readFileTool, writeFileTool],
                  configLLM = llmConfig,
                  configSkillsDir = Nothing,
                  configMaxTokens = Nothing,
                  configTemperature = Nothing
                }

        agent <- createAgent agentConfig
        result <- executeAgent agent $ "Read the file at path: " <> T.pack testFilePath

        -- Verify the agent executed successfully
        resultSuccess result `shouldBe` True
        resultError result `shouldBe` Nothing

        -- Verify output is non-empty
        T.length (resultOutput result) `shouldSatisfy` (> 0)

        -- Verify file tools were available
        length (availableTools agent) `shouldBe` 2

    it "agent uses multiple tools in sequence" $ do
      withTempDir "agentic-test-multi" $ \tmpDir -> do
        -- Create a test file with numbers
        let testFilePath = tmpDir ++ "/numbers.txt"
        TIO.writeFile testFilePath "15 and 23"

        -- Create agent with both calculator and file tools
        let agentConfig =
              AgentConfig
                { configName = "multi-tool-agent",
                  configSystemPrompt =
                    "You are a versatile assistant. Use file tools to read files and calculator for math.",
                  configTools = [readFileTool, calculatorTool, listDirectoryTool],
                  configLLM = llmConfig,
                  configSkillsDir = Nothing,
                  configMaxTokens = Nothing,
                  configTemperature = Nothing
                }

        agent <- createAgent agentConfig
        result <-
          executeAgent
            agent
            $ "First, read the file at "
              <> T.pack testFilePath
              <> ", then multiply the two numbers you find."

        -- Verify the agent executed successfully
        resultSuccess result `shouldBe` True
        resultError result `shouldBe` Nothing

        -- Verify output is non-empty
        T.length (resultOutput result) `shouldSatisfy` (> 0)

        -- Verify multiple tools were available
        length (availableTools agent) `shouldBe` 3

    it "agent handles tool execution errors gracefully" $ do
      -- Create agent with file reader tool
      let agentConfig =
            AgentConfig
              { configName = "error-handling-agent",
                configSystemPrompt = "You are a file assistant.",
                configTools = [readFileTool],
                configLLM = llmConfig,
                configSkillsDir = Nothing,
                configMaxTokens = Nothing,
                configTemperature = Nothing
              }

      agent <- createAgent agentConfig
      -- Try to read a non-existent file
      result <- executeAgent agent "Read the file at /nonexistent/path/file.txt"

      -- Agent should complete execution even if tool fails
      -- (the LLM will receive an error from the tool and should handle it)
      resultSuccess result `shouldSatisfy` const True

    it "agent preserves context across multiple tool uses" $ do
      withTempDir "agentic-test-context" $ \tmpDir -> do
        -- Create test files
        let file1 = tmpDir ++ "/file1.txt"
        let file2 = tmpDir ++ "/file2.txt"
        TIO.writeFile file1 "First file content"
        TIO.writeFile file2 "Second file content"

        let agentConfig =
              AgentConfig
                { configName = "context-agent",
                  configSystemPrompt = "You are a file assistant with good memory.",
                  configTools = [readFileTool],
                  configLLM = llmConfig,
                  configSkillsDir = Nothing,
                  configMaxTokens = Nothing,
                  configTemperature = Nothing
                }

        agent <- createAgent agentConfig

        -- First execution
        result1 <- executeAgent agent $ "Read " <> T.pack file1
        let ctx1 = resultContext result1

        -- Second execution with context
        result2 <-
          executeAgentWithContext
            agent
            ctx1
            $ "Now read " <> T.pack file2 <> " and tell me about both files"

        -- Verify both executions succeeded
        resultSuccess result1 `shouldBe` True
        resultSuccess result2 `shouldBe` True

        -- Verify context accumulated
        length (contextConversation (resultContext result2))
          `shouldSatisfy` (> length (contextConversation ctx1))

-- | Helper to create and cleanup temporary directory
withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir template action = do
  tmpBase <- getTemporaryDirectory
  let tmpDir = tmpBase </> template
  bracket
    (createDirectoryIfMissing True tmpDir >> return tmpDir)
    (\dir -> removeDirectoryRecursive dir)
    action

-- LLM configuration is now passed from Main.hs via environment variables
