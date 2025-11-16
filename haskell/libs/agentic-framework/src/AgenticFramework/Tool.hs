{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : AgenticFramework.Tool
Description : Tool system for agent capabilities
Copyright   : (c) 2025
License     : MIT

This module provides the tool system that allows agents to interact with
external systems, perform calculations, search the web, and more.

= Usage

@
import AgenticFramework.Tool
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A

-- Define a custom tool
weatherTool :: Tool
weatherTool = createTool ToolConfig
  { toolConfigName = "get_weather"
  , toolConfigDescription = "Get weather for a city"
  , toolConfigInputSchema = ...
  , toolConfigOutputSchema = ...
  , toolConfigExecute = \\input -> do
      -- Tool implementation
      return $ Right $ ToolOutput $ HM.fromList [...]
  , toolConfigTimeout = Just 5_000_000  -- 5 seconds
  , toolConfigRetryable = True
  }
@
-}

module AgenticFramework.Tool
  ( -- * Tool Creation and Execution
    createTool
  , executeTool

    -- * Built-in Tools (re-exported from sub-modules)
  , module AgenticFramework.Tool.File
  , module AgenticFramework.Tool.LangChain
  , module AgenticFramework.Tool.WebSearch

  ) where

import AgenticFramework.Types
import AgenticFramework.Tool.File
import AgenticFramework.Tool.LangChain
import AgenticFramework.Tool.WebSearch
import Data.Text (Text)
import Data.Aeson (Value)
import GHC.Generics (Generic)
import Control.Exception (try, SomeException)
import System.Timeout (timeout)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Tool Creation and Execution
--------------------------------------------------------------------------------

-- | Create a new tool from configuration.
--   Validates tool name and description are non-empty.
--   TODO: Add full JSON schema validation using hjsonschema library
createTool :: ToolConfig -> Tool
createTool config
  | T.null (toolConfigName config) = error "createTool: tool name cannot be empty"
  | T.null (toolConfigDescription config) = error "createTool: tool description cannot be empty"
  | otherwise = Tool
      { toolName = toolConfigName config
      , toolDescription = toolConfigDescription config
      , toolSchema = ToolSchema
          { inputSchema = toolConfigInputSchema config
          , outputSchema = toolConfigOutputSchema config
          }
      , toolExecute = toolConfigExecute config
      , toolTimeout = toolConfigTimeout config
      , toolRetryable = toolConfigRetryable config
      }

-- | Execute a tool with timeout and retry logic.
--   Returns Left ToolError on failure, Right ToolOutput on success.
--   Automatically retries on retryable errors up to 3 times.
executeTool :: Tool -> ToolInput -> IO (Either ToolError ToolOutput)
executeTool tool input = executeWithRetry 3
  where
    -- Default timeout: 10 seconds (10,000,000 microseconds)
    defaultTimeout = 10_000_000
    timeoutMicros = maybe defaultTimeout id (toolTimeout tool)
    maxRetries = if toolRetryable tool then 3 else 1

    -- Execute with retry logic
    executeWithRetry :: Int -> IO (Either ToolError ToolOutput)
    executeWithRetry attemptsLeft
      | attemptsLeft <= 0 = return $ Left $ ToolExecutionError "Maximum retry attempts exceeded"
      | otherwise = do
          result <- executeOnce
          case result of
            Right output -> return $ Right output
            Left err ->
              if toolRetryable tool && attemptsLeft > 1
              then executeWithRetry (attemptsLeft - 1)
              else return $ Left err

    -- Execute tool once with timeout
    executeOnce :: IO (Either ToolError ToolOutput)
    executeOnce = do
      -- TODO: Validate input against JSON schema

      -- Execute with timeout
      result <- timeout timeoutMicros $ try $ toolExecute tool input

      case result of
        Nothing -> return $ Left $ ToolTimeoutError
        Just (Left (err :: SomeException)) ->
          return $ Left $ ToolExecutionError $ T.pack $ show err
        Just (Right toolResult) -> do
          -- TODO: Validate output against JSON schema
          return toolResult
