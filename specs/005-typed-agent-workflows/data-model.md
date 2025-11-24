# Data Model: Typed Agent Workflows

**Feature**: 005-typed-agent-workflows
**Date**: 2025-11-16
**Based on**: [spec.md](./spec.md)

## Core Entities

### Agent

**Purpose**: Represents an AI assistant with system prompt, capabilities, tools, and optional workflow

**Fields:**
- `agentId`: Unique identifier (UUID)
- `agentName`: Human-readable name
- `agentSystemPrompt`: Base behavior/role description (Text)
- `agentTools`: List of available tools (Tool[])
- `agentCapabilities`: Natural language behavior modifiers (Capability[])
- `agentWorkflow`: Optional structured workflow (Maybe Workflow)
- `agentLLM`: LLM configuration (LLMConfig)
- `createdAt`: Timestamp of agent creation
- `modifiedAt`: Timestamp of last modification

**Relationships:**
- Has many Tools (0..n)
- Has many Capabilities (0..n)
- Has zero or one Workflow (0..1)
- References one LLMConfig (1)

**Validation Rules:**
- agentSystemPrompt must not be empty
- agentName must be unique within system
- At least one of (agentWorkflow, agentTools) must be present

**State Transitions:**
- Created → Configured → Ready → Executing → Complete

---

### Capability

**Purpose**: Natural language description that modifies agent behavior

**Fields:**
- `capabilityId`: Unique identifier
- `capabilityName`: Short identifier (e.g., "reasoning")
- `capabilityDescription`: Natural language behavior description
- `capabilityParameters`: Optional configuration (JSON)
- `capabilitySource`: Origin (builtin|file|dynamic)
- `capabilityVersion`: Version string for compatibility

**Relationships:**
- Can be used by many Agents (n..m)
- Can be applied to many WorkflowSteps (n..m)

**Validation Rules:**
- capabilityName must match pattern: [a-z][a-z0-9-]*
- capabilityDescription must be 10-500 characters
- capabilityParameters must be valid JSON if present

---

### Workflow

**Purpose**: Typed sequence of steps with data flow between them

**Fields:**
- `workflowId`: Unique identifier
- `workflowName`: Descriptive name
- `workflowDescription`: Purpose and overview
- `workflowSteps`: Ordered list of steps (WorkflowStep[])
- `workflowInputType`: Type signature of input data
- `workflowOutputType`: Type signature of output data
- `workflowMetadata`: Additional configuration (JSON)

**Relationships:**
- Belongs to zero or more Agents (0..n)
- Contains many WorkflowSteps (1..n)
- Can reference other Workflows (for nesting)

**Validation Rules:**
- Must have at least one step
- Input/output types must be compatible with connected steps
- No circular dependencies in nested workflows

**State Transitions:**
- Draft → Validated → Ready → Executing → Complete/Failed

---

### WorkflowStep

**Purpose**: Individual operation within a workflow (LLM call, tool use, sub-workflow)

**Fields:**
- `stepId`: Unique identifier within workflow
- `stepName`: Human-readable name
- `stepType`: Type of operation (llm_call|tool_use|sub_workflow|branch|parallel)
- `stepInputType`: Expected input type signature
- `stepOutputType`: Produced output type signature
- `stepOperation`: Operation details (varies by type)
- `stepCapabilities`: Capabilities applied to this step (Capability[])
- `stepTimeout`: Optional timeout in milliseconds
- `stepRetryPolicy`: Retry configuration

**Relationships:**
- Belongs to one Workflow (1)
- May use multiple Capabilities (0..n)
- May reference one Tool (0..1)
- May reference another Workflow (for sub-workflow type)

**Validation Rules:**
- stepInputType must match previous step's outputType (or workflow input)
- stepOutputType must match next step's inputType (or workflow output)
- Tool reference must exist if stepType is tool_use
- Capabilities must be available in agent context

---

### Tool

**Purpose**: External function that can be invoked by agents (from LangChain or custom)

**Fields:**
- `toolId`: Unique identifier
- `toolName`: Invocation name
- `toolDescription`: Human-readable description
- `toolInputSchema`: JSON Schema for input validation
- `toolOutputSchema`: JSON Schema for output format
- `toolProvider`: Source (langchain|custom|external)
- `toolTimeout`: Execution timeout in milliseconds
- `toolRetryable`: Whether tool supports retry on failure
- `toolCost`: Optional cost metric for usage tracking

**Relationships:**
- Can be used by many Agents (n..m)
- Can be invoked by many WorkflowSteps (n..m)

**Validation Rules:**
- toolName must be unique within agent context
- toolInputSchema must be valid JSON Schema
- toolTimeout must be between 100ms and 300000ms (5 minutes)

---

### AgentContext

**Purpose**: Runtime state including conversation history, current prompt, and execution state

**Fields:**
- `contextId`: Session identifier
- `contextSystemPrompt`: Active system prompt
- `contextUserPrompt`: Current user input
- `contextTools`: Available tools in this context
- `contextCapabilities`: Active capabilities
- `contextLLM`: LLM configuration for this session
- `contextHistory`: Conversation history (Message[])
- `contextState`: Mutable execution state
- `contextTokenCount`: Current token usage
- `contextMaxTokens`: Token limit

**Relationships:**
- Belongs to one Agent execution (1)
- Contains many Messages (0..n)
- References Tools and Capabilities from Agent

**Validation Rules:**
- contextTokenCount must not exceed contextMaxTokens
- contextHistory must maintain chronological order

**State Transitions:**
- Initialize → Active → Suspended → Resumed → Complete

---

## Type Signatures

### Core Types

```haskell
-- Workflow input/output types
type WorkflowInput = Value  -- JSON Value for flexibility
type WorkflowOutput = Value

-- Step connection types
data StepConnection = StepConnection
  { fromStep :: StepId
  , toStep :: StepId
  , dataTransform :: Maybe (Value -> Value)
  }

-- Execution result types
data ExecutionResult
  = Success Value
  | Failure WorkflowError
  | Partial Value WorkflowError  -- Partial success with error
```

### Error Types

```haskell
data WorkflowError
  = ToolNotFound Text
  | CapabilityMissing Text
  | TypeError Text Text  -- Expected, Actual
  | TimeoutError StepId
  | ValidationError Text
  | CircularDependency [StepId]
```

## Persistence Strategy

### Configuration Storage
- **Agents**: JSON files in `~/.config/agentic-framework/agents/`
- **Capabilities**: JSON files in `~/.config/agentic-framework/capabilities/`
- **Workflows**: JSON files in `~/.config/agentic-framework/workflows/`

### Runtime Storage
- **AgentContext**: In-memory with optional Redis backing for distribution
- **Execution History**: Append-only log files in `~/.local/share/agentic-framework/logs/`
- **Cache**: LRU cache for capability definitions with 24-hour TTL

### Schema Versioning
- All persisted entities include version field
- Migration scripts for schema upgrades
- Backward compatibility for one major version

## Access Patterns

### Common Queries
1. Load agent by name with all capabilities and tools
2. Find workflows compatible with given input type
3. List all capabilities applicable to a step type
4. Get execution history for debugging
5. Validate workflow before execution

### Performance Considerations
- Capability definitions cached in memory after first load
- Workflow validation results cached until modification
- Tool schemas validated once at startup
- Lazy loading of conversation history (pagination)

## Data Constraints Summary

| Entity | Key Constraint | Performance Impact |
|--------|---------------|-------------------|
| Agent | Unique name | Index on name |
| Capability | Pattern-matched name | Cache all in memory |
| Workflow | No circular deps | Validate on save |
| WorkflowStep | Type compatibility | Compile-time check |
| Tool | Unique name per agent | Hash lookup |
| AgentContext | Token limit | Check per operation |