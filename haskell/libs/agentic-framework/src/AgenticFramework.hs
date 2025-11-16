{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : AgenticFramework
Description : Main entry point for the Agentic Framework library
Copyright   : (c) 2025
License     : MIT
Maintainer  : maintainer@example.com

This is the main module for the Agentic Framework library, providing
autonomous agents with tool access, skill loading, multi-agent orchestration,
and comprehensive observability.

= Quick Start

@
import AgenticFramework

-- Create a simple agent
main :: IO ()
main = do
  let agent = createAgent "my-agent" "You are a helpful assistant" []
  result <- executeAgent agent "Hello, how are you?"
  print result
@

= Main Features

* 'Agent' - Autonomous reasoning entity with tools and skills
* 'Tool' - Capabilities agents can invoke
* 'Skill' - Reusable methodologies from markdown files
* 'Orchestration' - Multi-agent coordination (sequential, parallel, agent-as-tool)
* 'Context' - Token management and context window handling
* 'Logging' - Comprehensive observability and structured logging

= Architecture

The library is organized into several main components:

* "AgenticFramework.Agent" - Agent creation and execution
* "AgenticFramework.Tool" - Tool system and built-in tools
* "AgenticFramework.Skill" - Skill loading and management
* "AgenticFramework.Orchestration" - Multi-agent workflows
* "AgenticFramework.Context" - Context and token management
* "AgenticFramework.Logging" - Logging and observability
* "AgenticFramework.Types" - Core type definitions

-}

module AgenticFramework
  ( -- * Core Types
    -- ** Agent
    Agent
  , AgentId
  , AgentConfig
  , AgentResult

    -- ** Tool
  , Tool
  , ToolInput
  , ToolOutput
  , ToolError (..)
  , ToolSchema

    -- ** Skill
  , Skill
  , SkillLabel
  , SkillCategory
  , SkillContent

    -- ** Context
  , AgentContext
  , TokenMetrics

    -- ** Orchestration
  , HandoffObject
  , HandoffMetadata
  , ExecutionStatus (..)

    -- ** Logging
  , ExecutionLog
  , LogEntry
  , LogLevel (..)
  , LogHandler (..)

    -- * Agent Operations
    -- $agent-operations
    -- Re-exported from "AgenticFramework.Agent"

    -- * Tool Operations
    -- $tool-operations
    -- Re-exported from "AgenticFramework.Tool"

    -- * Skill Operations
    -- $skill-operations
    -- Re-exported from "AgenticFramework.Skill"

    -- * Orchestration Operations
    -- $orchestration-operations
    -- Re-exported from "AgenticFramework.Orchestration"

    -- * Context Operations
    -- $context-operations
    -- Re-exported from "AgenticFramework.Context"

    -- * Logging Operations
    -- $logging-operations
    -- Re-exported from "AgenticFramework.Logging"

  ) where

-- Note: This is a skeleton. Actual re-exports will be added as modules are implemented.
-- For now, this establishes the public API surface.

-- The following imports will be uncommented as modules are implemented:
-- import AgenticFramework.Agent
-- import AgenticFramework.Tool
-- import AgenticFramework.Skill
-- import AgenticFramework.Orchestration
-- import AgenticFramework.Context
-- import AgenticFramework.Logging
-- import AgenticFramework.Types

-- Placeholder types until actual implementations exist
data Agent
data AgentId
data AgentConfig
data AgentResult

data Tool
data ToolInput
data ToolOutput
data ToolError = ToolExecutionError | ToolTimeoutError | ToolValidationError | ToolAuthorizationError
data ToolSchema

data Skill
data SkillLabel
data SkillCategory
data SkillContent

data AgentContext
data TokenMetrics

data HandoffObject
data HandoffMetadata
data ExecutionStatus = Success | PartialSuccess | Failed

data ExecutionLog
data LogEntry
data LogLevel = DEBUG | INFO | WARN | ERROR

class LogHandler h where
  logEntry :: h -> LogEntry -> IO ()

{- $agent-operations

Agent operations provide the core functionality for creating and executing
autonomous agents:

* 'createAgent' - Create a new agent with configuration
* 'executeAgent' - Execute an agent with input
* 'executeAgentWithContext' - Execute with existing context for multi-turn conversations

-}

{- $tool-operations

Tool operations enable agents to interact with external systems:

* 'createTool' - Define a custom tool
* 'executeTool' - Execute a tool with input
* Built-in tools: file operations, web search, calculator

-}

{- $skill-operations

Skill operations allow agents to access reusable methodologies:

* 'loadSkillsFromDirectory' - Discover and load skills
* 'loadSkillContent' - Load full skill content on-demand
* 'searchSkills' - Find skills by label, category, or keyword
* 'addSkillToAgent' - Make a skill available to an agent

-}

{- $orchestration-operations

Orchestration operations coordinate multiple agents:

* 'executeSequential' - Sequential workflow with handoffs
* 'executeParallel' - Parallel agent execution
* 'createAgentTool' - Wrap an agent as a tool (agent-as-tool pattern)

-}

{- $context-operations

Context operations manage token usage and context windows:

* 'getTokenMetrics' - Read-only access to token metrics
* Automatic summarization at 90% threshold
* Context preservation across multi-turn conversations

-}

{- $logging-operations

Logging operations provide observability:

* 'runWithLogging' - Execute agent with logging
* 'filterLogs' - Filter logs by agent, tool, level, time
* 'exportLogsJSON' - Export logs as JSON
* 'exportLogsText' - Export logs as human-readable text

-}
