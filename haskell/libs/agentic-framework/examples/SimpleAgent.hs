{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : SimpleAgent
Description : Example demonstrating a simple agent with tools
Copyright   : (c) 2025
License     : MIT

This example demonstrates creating and executing a simple agent with
calculator and file reader tools.

= Usage

@
bazel run //haskell/libs/agentic-framework/examples:simple-agent
@

-}

module Main (main) where

import AgenticFramework.Agent
import AgenticFramework.Tool
import AgenticFramework.Tool.File
import AgenticFramework.Tool.LangChain
import AgenticFramework.Types
import AgenticFramework.Context (AgentContext(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  putStrLn "=== Simple Agent Example ==="
  putStrLn ""

  -- Create agent with file and calculator tools
  agent <- createSimpleAgent

  putStrLn $ "Created agent: " ++ T.unpack (agentName agent)
  putStrLn $ "Available tools: " ++ show (length (availableTools agent))
  putStrLn ""

  -- Example 1: Simple query
  putStrLn "Example 1: Simple query"
  result1 <- executeAgent agent "What is 2 + 2?"
  displayResult result1
  putStrLn ""

  -- Example 2: File reading task (using stub)
  putStrLn "Example 2: File reading task"
  result2 <- executeAgent agent "Please read the file README.md"
  displayResult result2
  putStrLn ""

  -- Example 3: Multi-turn conversation
  putStrLn "Example 3: Multi-turn conversation"
  result3 <- executeAgent agent "Hello, what can you help me with?"
  displayResult result3

  let ctx = resultContext result3
  result4 <- executeAgentWithContext agent ctx "Can you calculate 10 * 5?"
  displayResult result4
  putStrLn ""

  putStrLn "=== Example Complete ==="

-- | Create a simple agent with calculator and file tools
createSimpleAgent :: IO Agent
createSimpleAgent = do
  let llmCfg = LLMConfig
        { llmProvider = Ollama
        , llmModel = "llama2"
        , llmApiKey = Nothing
        , llmBaseUrl = Just "http://localhost:11434"
        , llmMaxTokens = 4096
        , llmTemperature = 0.7
        }

  let config = AgentConfig
        { configName = "simple-agent"
        , configSystemPrompt = "You are a helpful assistant with access to a calculator and file reader. Help users with calculations and reading files."
        , configTools = [calculatorTool, readFileTool, listDirectoryTool]
        , configLLM = llmCfg
        , configSkillsDir = Nothing
        , configMaxTokens = Nothing
        , configTemperature = Nothing
        }

  createAgent config

-- | Display agent execution result
displayResult :: AgentResult -> IO ()
displayResult result = do
  putStrLn $ "Success: " ++ show (resultSuccess result)

  case resultError result of
    Just err -> putStrLn $ "Error: " ++ T.unpack err
    Nothing -> return ()

  putStrLn $ "Output: " ++ T.unpack (resultOutput result)
  putStrLn $ "Tools used: " ++ show (length (resultToolsUsed result))

  -- Show conversation length
  let ctx = resultContext result
  putStrLn $ "Conversation messages: " ++ show (length (contextConversation ctx))
