{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.DSL
-- Description : DSL for defining agent workflows
-- Copyright   : (c) 2025
-- License     : MIT
module AgenticFramework.Workflow.DSL
  ( -- * Workflow Actions
    llmCall,
    useTool,
    getUserPrompt,

    -- * Control Flow
    branch,

    -- * Parallel Execution
    parallel,

    -- * Capability Management
    withCapability,
  )
where

import AgenticFramework.Workflow (runWorkflow)
import AgenticFramework.Workflow.Types
import AgenticFramework.Workflow.Capabilities (applyCapabilities)
import AgenticFramework.Types (Tool (..), ToolInput (..), ToolOutput (..), ToolError (..), LLMConfig (..), LLMProvider (..))
import AgenticFramework.Tool (executeTool)
import qualified AgenticFramework.LLM.Ollama as Ollama
import qualified AgenticFramework.LLM.Kimi as Kimi
import qualified Langchain.LLM.Core as LLM
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List

-- | Call the LLM with a prompt
--   Uses the agent's configured LLM
--   Applies any active capabilities from the current workflow state
llmCall :: Text -> Workflow Text
llmCall prompt = do
  ctx <- ask
  state <- get
  let config = ctxLLM ctx

  -- Apply active capabilities to modify the prompt
  let activeCaps = stActiveCapabilities state
      modifiedPrompt = applyCapabilities activeCaps prompt

  result <- liftIO $ case llmProvider config of
    Ollama -> do
      let llm = Ollama.createOllamaLLM config
      LLM.generate llm modifiedPrompt Nothing
    Kimi -> do
       case Kimi.createKimiLLM config of
         Nothing -> return $ Left "Kimi API key missing"
         Just llm -> LLM.generate llm modifiedPrompt Nothing
    Custom name
      | name == "Kimi" || name == "kimi" -> do
          -- Custom "Kimi" uses same backend as Kimi provider
          case Kimi.createKimiLLM config of
            Nothing -> return $ Left "Kimi API key missing"
            Just llm -> LLM.generate llm modifiedPrompt Nothing
      | otherwise -> return $ Left $ "Custom provider not supported: " ++ T.unpack name
    _ -> return $ Left "Provider not supported in DSL yet"

  case result of
    Left err -> error $ "LLM call failed: " ++ err
    Right response -> return response

-- | Use a tool by name
useTool :: Text -> Value -> Workflow Value
useTool name input = do
  ctx <- ask
  case List.find (\t -> toolName t == name) (ctxTools ctx) of
    Nothing -> error $ "Tool not found: " ++ show name
    Just tool -> do
      result <- liftIO $ executeTool tool (ToolInput input)
      case result of
        Left err -> error $ "Tool execution failed: " ++ show err
        Right (ToolOutput output) -> return output

-- | Get the user's prompt from context
getUserPrompt :: Workflow Text
getUserPrompt = do
  ctx <- ask
  return $ ctxUserPrompt ctx

-- | Branch based on a monadic condition
branch :: Workflow Bool -> Workflow a -> Workflow a -> Workflow a
branch condAction trueAction falseAction = do
  cond <- condAction
  if cond
    then trueAction
    else falseAction

-- | Execute multiple workflows in parallel
parallel :: [Workflow a] -> Workflow [a]
parallel workflows = do
  ctx <- ask
  state <- get
  results <- liftIO $ mapConcurrently (\w -> runWorkflow w ctx state) workflows
  return results

-- | Apply a capability to a workflow action (step-level capability)
--   This temporarily adds the capability to the active capabilities stack
--   and removes it after the action completes (capability isolation)
--   If the capability is not found, the action runs without it (graceful degradation)
withCapability :: Text -> Workflow a -> Workflow a
withCapability capabilityName action = do
  ctx <- ask
  state <- get

  -- Find the capability by name from context
  case List.find (\c -> capName c == capabilityName) (ctxCapabilities ctx) of
    Nothing ->
      -- Capability not found - just run the action without it
      action
    Just cap -> do
      -- Push capability onto the active stack
      let oldCaps = stActiveCapabilities state
      modify $ \s -> s { stActiveCapabilities = cap : oldCaps }

      -- Execute the action with the capability active
      result <- action

      -- Pop capability from the stack (restore previous state)
      modify $ \s -> s { stActiveCapabilities = oldCaps }

      return result
