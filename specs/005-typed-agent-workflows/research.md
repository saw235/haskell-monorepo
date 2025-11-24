# Research Findings: Typed Agent Workflows

**Feature**: 005-typed-agent-workflows
**Date**: 2025-11-16
**Status**: Complete

## Executive Summary

Research conducted on four key areas for implementing typed agent workflows in Haskell, with concrete recommendations based on existing codebase patterns and current best practices.

## 1. Runtime Capability Loading

### Decision: Aeson (JSON) with File-Based Loading

**Rationale:**
- Already integrated in codebase (aeson 2.2.3.0)
- Proven caching patterns in HackageClient module
- Best performance and type safety for JSON
- Simple deployment (JSON files in config directory)

**Alternatives Considered:**
- **Dhall**: More powerful but unnecessary complexity for capability definitions
- **YAML**: Less type-safe, harder to validate
- **Environment Variables**: Only for secrets, not structured data

**Implementation Approach:**
```haskell
-- Capability definitions in JSON files
{
  "name": "reasoning",
  "description": "Break down complex problems step-by-step",
  "parameters": {
    "style": "chain-of-thought",
    "maxSteps": 10
  }
}
```

Files will be loaded from `~/.config/agentic-framework/capabilities/` with 24-hour cache TTL.

## 2. Workflow Type Safety Pattern

### Decision: Indexed Monads with GADTs

**Rationale:**
- Compile-time verification of workflow phase transitions
- Cannot skip steps or execute out of order
- Existing foundation in AgenticFramework.Capabilities.hs
- Clear type-level documentation

**Alternatives Considered:**
- **Phantom Types**: Less explicit than indexed monads
- **Complex Transformer Stacks**: Order dependencies cause bugs
- **Streamly**: Overkill for non-streaming workflows

**Implementation Approach:**
```haskell
-- Type-safe workflow phases
data WorkflowPhase = Init | Configured | Ready | Executing | Complete

-- Indexed workflow monad
newtype WorkflowM (i :: WorkflowPhase) (j :: WorkflowPhase) a

-- Phase transitions enforced at compile time
configure :: WorkflowM 'Init 'Configured Config
execute :: WorkflowM 'Ready 'Executing Result
```

## 3. Error Handling Strategy

### Decision: ReaderT + IO with Explicit Either

**Rationale:**
- ExceptT over IO has critical issues (no stack traces, resource leaks)
- StateT loses state on exceptions unpredictably
- ReaderT is the only predictable transformer
- Modern Haskell moving away from complex stacks

**Alternatives Considered:**
- **ExceptT/StateT/WriterT Stack**: Unpredictable, loses state
- **effectful Library**: Good alternative but adds dependency
- **MTL Style**: Works but has known composition issues

**Implementation Approach:**
```haskell
-- Simple, predictable stack
newtype AgentM a = AgentM (ReaderT AgentEnv IO a)

-- Explicit error handling
executeWorkflow :: Workflow -> AgentM (Either WorkflowError Result)

-- Mutable state via IORef
data AgentEnv = AgentEnv
  { envState :: IORef AgentState
  , envConfig :: Config
  }
```

## 4. LangChain Integration

### Decision: Abstract Tool Invocation Layer

**Rationale:**
- Existing Tool type is well-designed and provider-agnostic
- Need adapter pattern to bridge langchain-hs
- Maintain flexibility for multiple LLM providers
- Leverage langchain-hs Chain composition

**Alternatives Considered:**
- **Direct langchain-hs Coupling**: Too restrictive
- **Complete Reimplementation**: Duplicates effort
- **Abandon Tool Type**: Would break existing code

**Implementation Approach:**
```haskell
-- Abstract interface
class ToolExecutor m where
  invokeTool :: Tool -> ToolInput -> m (Either ToolError ToolOutput)

-- Provider implementations
instance ToolExecutor IO  -- Direct execution
instance (LLM llm) => ToolExecutor (llm -> IO)  -- Via langchain-hs
```

## Key Technical Decisions Summary

| Area | Choice | Impact |
|------|--------|--------|
| Configuration Format | JSON via Aeson | Leverages existing infrastructure |
| Type Safety | Indexed Monads + GADTs | Compile-time workflow verification |
| Error Handling | ReaderT + IO + Either | Predictable, no state loss |
| Tool Abstraction | Typeclass-based | Provider flexibility |
| State Management | IORef in ReaderT | Safe mutable state |
| Capability Storage | File-based with caching | Simple deployment |
| LLM Integration | Adapter pattern | Maintains compatibility |

## Implementation Priority

1. **Phase 1**: Core workflow monad with indexed types
2. **Phase 2**: Capability loading and caching system
3. **Phase 3**: Tool abstraction layer
4. **Phase 4**: LangChain-hs integration adapters

## Resolved Clarifications

**FR-015 Resolution**: Capabilities will be loadable from JSON files with optional environment variable overrides for specific parameters (e.g., API keys). Primary mechanism is file-based for complex definitions, environment variables for runtime overrides.

All technical unknowns have been researched and resolved. Ready to proceed with implementation.