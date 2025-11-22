{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Builder
-- Description : DSL for building agents with workflows and capabilities
-- Copyright   : (c) 2025
-- License     : MIT
module AgenticFramework.Workflow.Builder
  ( -- * Agent Builder Monad
    AgentBuilder,
    buildAgent,

    -- * Configuration Functions
    withSystemPrompt,
    withLLM,
    withTool,
    withCapability,
    withWorkflow,
    withSkillsDir,
    withMaxTokens,
    withTemperature,
  )
where

import AgenticFramework.Agent (Agent, AgentConfig (..), createAgent)
import AgenticFramework.Types (LLMConfig, Tool)
import AgenticFramework.Workflow.Types (Capability, Workflow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, execStateT, modify)
import Data.Text (Text)

-- | Agent Builder Monad
type AgentBuilder = StateT AgentConfig IO

-- | Build an agent using the builder DSL
buildAgent :: AgentBuilder () -> IO Agent
buildAgent builder = do
  -- Default configuration
  let defaultConfig =
        AgentConfig
          { configName = "agent",
            configSystemPrompt = "You are a helpful assistant.",
            configTools = [],
            configLLM = undefined, -- Must be provided
            configSkillsDir = Nothing,
            configMaxTokens = Nothing,
            configTemperature = Nothing,
            configCapabilities = [],
            configWorkflow = Nothing
          }

  finalConfig <- execStateT builder defaultConfig
  createAgent finalConfig

-- | Set the system prompt
withSystemPrompt :: Text -> AgentBuilder ()
withSystemPrompt prompt = modify $ \c -> c {configSystemPrompt = prompt}

-- | Set the LLM configuration
withLLM :: LLMConfig -> AgentBuilder ()
withLLM llm = modify $ \c -> c {configLLM = llm}

-- | Add a tool to the agent
withTool :: Tool -> AgentBuilder ()
withTool tool = modify $ \c -> c {configTools = configTools c ++ [tool]}

-- | Add a capability to the agent
withCapability :: Capability -> AgentBuilder ()
withCapability cap = modify $ \c -> c {configCapabilities = configCapabilities c ++ [cap]}

-- | Set the workflow
withWorkflow :: Workflow Text -> AgentBuilder ()
withWorkflow wf = modify $ \c -> c {configWorkflow = Just wf}

-- | Set the skills directory
withSkillsDir :: FilePath -> AgentBuilder ()
withSkillsDir dir = modify $ \c -> c {configSkillsDir = Just dir}

-- | Set max tokens
withMaxTokens :: Int -> AgentBuilder ()
withMaxTokens maxT = modify $ \c -> c {configMaxTokens = Just maxT}

-- | Set temperature
withTemperature :: Double -> AgentBuilder ()
withTemperature temp = modify $ \c -> c {configTemperature = Just temp}
