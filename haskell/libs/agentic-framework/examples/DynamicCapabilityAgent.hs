{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : DynamicCapabilityAgent
-- Description : Example agent that loads capabilities from JSON files
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This example demonstrates User Story 5: loading and composing
-- agent capabilities dynamically from JSON configuration files.
module Main where

import AgenticFramework.Agent (executeAgent, resultOutput, agentCapabilities)
import AgenticFramework.Types (LLMConfig (..), LLMProvider (..))
import AgenticFramework.Workflow.Builder
import AgenticFramework.Workflow.Capabilities (applyCapabilities, composeCapabilities, mergeCapabilities)
import AgenticFramework.Workflow.Loader
import AgenticFramework.Workflow.Types (Capability (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.FilePath ((</>))

-- | Default path to example capabilities
exampleCapabilitiesDir :: FilePath
exampleCapabilitiesDir = "haskell/libs/agentic-framework/examples/capabilities"

main :: IO ()
main = do
  args <- getArgs
  let capDir = case args of
        (dir:_) -> dir
        [] -> exampleCapabilitiesDir

  putStrLn "=== Dynamic Capability Loading Example ==="
  putStrLn $ "Loading capabilities from: " ++ capDir
  putStrLn ""

  -- Load all capabilities from the directory
  (errors, capabilities) <- loadCapabilitiesFromDirectory capDir

  -- Report any loading errors
  if not (null errors)
    then do
      putStrLn "Warning: Some capabilities failed to load:"
      mapM_ (putStrLn . ("  - " ++) . show) errors
      putStrLn ""
    else pure ()

  -- Report loaded capabilities
  putStrLn $ "Successfully loaded " ++ show (length capabilities) ++ " capabilities:"
  mapM_ printCapability capabilities
  putStrLn ""

  -- Demonstrate capability application
  putStrLn "=== Capability Application Demo ==="
  let testPrompt = "What is the capital of France?"
  putStrLn $ "Original prompt: " ++ T.unpack testPrompt
  putStrLn ""

  -- Apply each capability individually
  mapM_ (demonstrateCapability testPrompt) capabilities

  -- Demonstrate capability composition
  putStrLn "=== Capability Composition Demo ==="
  case capabilities of
    (cap1:cap2:_) -> do
      let composed = composeCapabilities cap1 cap2
      putStrLn $ "Composing: " ++ T.unpack (capName cap1) ++ " + " ++ T.unpack (capName cap2)
      putStrLn $ "Result: " ++ T.unpack (capModifier composed testPrompt)
      putStrLn ""
    _ -> putStrLn "Need at least 2 capabilities for composition demo"

  -- Demonstrate merged capabilities
  case mergeCapabilities capabilities of
    Just merged -> do
      putStrLn $ "All capabilities merged into: " ++ T.unpack (capName merged)
      putStrLn $ "Applied to prompt: " ++ T.unpack (capModifier merged testPrompt)
    Nothing -> putStrLn "No capabilities to merge"

  putStrLn ""
  putStrLn "=== Building Agent with Loaded Capabilities ==="

  -- Build an agent with the dynamically loaded capabilities
  agent <- buildAgent $ do
    withSystemPrompt "You are a helpful AI assistant."
    mapM_ withCapability capabilities
    withLLM $ LLMConfig
      { llmProvider = Custom "Kimi"
      , llmModel = "moonshot-v1-8k"
      , llmApiKey = Nothing
      , llmBaseUrl = Just "https://api.moonshot.cn/v1"
      , llmMaxTokens = 2000
      , llmTemperature = 0.7
      }

  putStrLn $ "Agent built with " ++ show (length (agentCapabilities agent)) ++ " capabilities"
  putStrLn "Done!"

-- | Print capability info
printCapability :: Capability -> IO ()
printCapability cap = do
  putStrLn $ "  - " ++ T.unpack (capName cap) ++ ": " ++ T.unpack (capDescription cap)

-- | Demonstrate a single capability
demonstrateCapability :: Text -> Capability -> IO ()
demonstrateCapability prompt cap = do
  putStrLn $ "Applying '" ++ T.unpack (capName cap) ++ "':"
  let modified = capModifier cap prompt
  -- Show first 100 chars to keep output readable
  let preview = if T.length modified > 100
                then T.take 100 modified <> "..."
                else modified
  TIO.putStrLn $ "  " <> preview
  putStrLn ""
