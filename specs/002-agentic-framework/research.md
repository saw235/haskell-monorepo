# Research: Agentic Framework Library

**Date**: 2025-11-15
**Feature**: 002-agentic-framework
**Purpose**: Resolve technical unknowns and document architectural decisions for implementation

## 1. Rust FFI Tokenizer Integration

### Decision
Use **tiktoken-rs** (Rust port of OpenAI's tiktoken) via Haskell FFI with C bindings.

### Rationale
- **Accuracy**: tiktoken is the reference implementation for GPT models; tiktoken-rs maintains compatibility
- **Multi-model support**: Supports GPT-4, GPT-3.5, Claude (via cl100k_base), and custom tokenizers
- **Performance**: Rust implementation is faster than pure Haskell would be; <10ms tokenization typical
- **Maintenance**: Active maintenance, tracks OpenAI model updates
- **FFI simplicity**: Can expose simple C API: `count_tokens(model_name: *const char, text: *const char) -> i32`

### Alternatives Considered
1. **HuggingFace tokenizers (Rust)**: More general but heavier dependency; overkill for token counting only
2. **Pure Haskell BPE**: ~2000 LOC, ongoing maintenance burden, accuracy verification challenges
3. **Character heuristics (1 token ≈ 4 chars)**: Cannot meet 5% accuracy requirement; models vary significantly

### Implementation Approach
```haskell
-- Context/Tokenizer.hs
foreign import ccall "tiktoken_count_tokens"
  c_tiktoken_count_tokens :: CString -> CString -> IO CInt

countTokens :: ModelName -> Text -> IO Int
countTokens model text = ...
```

Bazel integration via `cc_library` for Rust compiled to C static library, linked in haskell_library rule.

---

## 2. langchain-hs Integration Strategy

### Decision
Wrap existing langchain-hs components without forking; build orchestration layer on top.

### Rationale
- **Leverage existing work**: langchain-hs provides LLM providers (Ollama, OpenAI), ReAct agent, basic tools
- **Avoid duplication**: Don't re-implement LLM communication, API handling
- **Add value at higher level**: Focus on orchestration, context management, observability—areas langchain-hs doesn't address
- **Type safety**: Wrap langchain-hs types in our own types for better error messages and API control

### Alternatives Considered
1. **Fork langchain-hs**: High maintenance burden, divergence from upstream
2. **Direct LLM integration**: Duplicates existing work, larger scope
3. **Wrap AutoGPT/LangGraph equivalents**: No mature Haskell implementations exist

### Integration Points
```haskell
-- Wrap langchain-hs ReAct agent as execution engine
import qualified LangChain.Agents.ReAct as LC
import qualified LangChain.LLMs as LC

data Agent = Agent
  { agentId :: AgentId
  , systemPrompt :: Text
  , llmConfig :: LC.LLMConfig  -- Wrapped langchain config
  , tools :: [Tool]
  , ...
  }

executeAgent :: Agent -> Text -> IO AgentResult
executeAgent agent input = do
  -- Convert our Tool type to LC.Tool
  -- Execute via LC.runReActAgent
  -- Wrap result in our AgentResult type
```

---

## 3. Context Summarization Strategy

### Decision
Use the agent's own LLM to summarize conversation history with structured prompt.

### Rationale
- **Self-contained**: No external summarization model needed
- **Context-aware**: Agent understands its own reasoning better than external summarizer
- **Type-aligned**: Summary preserves agent's "mental model" for task
- **Simple implementation**: Just another LLM call with specific prompt

### Alternatives Considered
1. **Rule-based truncation**: Loses important mid-conversation context
2. **Separate summarization model**: Additional dependency, different "voice"
3. **Vector embedding similarity**: Complex, requires vector store, overkill for this use case

### Summarization Prompt Template
```haskell
summarizationPrompt :: [Message] -> Text
summarizationPrompt oldMessages = T.unlines
  [ "You are summarizing your own conversation history to save space."
  , "Preserve ONLY:"
  , "- Key decisions you made"
  , "- Important facts you discovered"
  , "- Your current task state"
  , "- Relevant tool outputs (file contents, search results, etc.)"
  , ""
  , "OMIT:"
  , "- Reasoning steps (keep conclusions only)"
  , "- Repetitive tool calls"
  , "- Conversational fluff"
  , ""
  , "Conversation to summarize:"
  , renderMessages oldMessages
  , ""
  , "Concise summary:"
  ]
```

Target: 50%+ reduction (FR-042), verified via property-based test.

---

## 4. Skill File Format and Discovery

### Decision
Front-matter style with YAML-like headers, pure markdown content.

### Rationale
- **Human-readable**: Standard markdown, easy to edit
- **Machine-parseable**: Simple header extraction
- **Lazy loading**: Headers loaded first (label, description, category), content on-demand
- **Flexibility**: Content structure recommended but not enforced

### Format Specification
```markdown
---
label: debugging-checklist
description: Systematic approach to debugging runtime errors
category: debugging
---

## Purpose
Methodically isolate and fix bugs in production systems...

## Methodology
1. Reproduce the issue
2. Check logs for error messages
3. ...

## Examples
### Example 1: NullPointerException
...
```

Parsing: Split on `---\n`, parse YAML-style headers, treat rest as markdown content.

### Alternatives Considered
1. **Pure YAML with embedded markdown**: Less readable in text editors
2. **JSON metadata file + .md content**: File proliferation
3. **No structure, full content always loaded**: Context window waste

---

## 5. Parallel Agent Execution Model

### Decision
Use Haskell `async` library for concurrent agent execution with result collection.

### Rationale
- **Native concurrency**: Lightweight Haskell threads, good for I/O-bound LLM calls
- **Composable**: `async`/`wait`/`waitAny` patterns well-established
- **Error handling**: Each agent failure captured independently
- **Resource control**: Can set max concurrent via semaphore if needed

### Alternatives Considered
1. **STM for coordination**: Overkill; simple async sufficient
2. **External task queue**: Unnecessary infrastructure for library
3. **Sequential with manual threading**: Reinventing `async`

### Implementation Pattern
```haskell
executeParallel :: [Agent] -> Text -> IO [AgentResult]
executeParallel agents input = do
  asyncTasks <- mapM (\a -> async (executeAgent a input)) agents
  results <- mapM wait asyncTasks
  return results
```

Linear scaling target (SC-006) achievable due to I/O-bound nature (LLM API calls dominate).

---

## 6. Logging Architecture: monad-logger + Handlers

### Decision
Core logging via monad-logger's `LoggingT` monad transformer; pluggable backends via custom `LogHandler` typeclass.

### Rationale
- **Ecosystem integration**: monad-logger is Haskell standard (FR-029a)
- **Flexibility**: Custom handlers for non-standard backends (FR-029b)
- **Colorized stdout**: ansi-terminal for developer UX (FR-029c)
- **Structured data**: Log entries carry typed metadata (agent ID, tool name, timestamp)

### Alternatives Considered
1. **Only monad-logger**: Can't meet custom handler requirement (FR-029b)
2. **Only custom handlers**: Reinventing monad-logger, poor ecosystem integration
3. **Multiple logging libraries**: Confusion, version conflicts

### Architecture
```haskell
-- Default: monad-logger backend
runAgentWithLogging :: Agent -> LoggingT IO AgentResult

-- Custom handlers via typeclass
class LogHandler h where
  logEntry :: h -> LogEntry -> IO ()

data LogEntry = LogEntry
  { level :: LogLevel
  , agentId :: Maybe AgentId
  , toolName :: Maybe Text
  , message :: Text
  , timestamp :: UTCTime
  , metadata :: HashMap Text Value
  }

-- Colorized stdout handler
instance LogHandler ColorizedStdout where
  logEntry h entry = do
    let colored = colorize (level entry) (message entry)
    putStrLn colored
```

---

## 7. HandoffObject Serialization

### Decision
Primary format: JSON (via aeson); secondary format: structured text for prompt injection.

### Rationale
- **Programmatic access**: JSON for developers modifying handoff (FR-023a)
- **Prompt injection**: Text format for agent consumption
- **Type safety**: Aeson provides encode/decode with validation
- **Extensibility**: customFields allow workflow-specific data

### Alternatives Considered
1. **Only JSON**: Awkward for prompt injection (agents see raw JSON)
2. **Only text**: No programmatic access for developers
3. **Protocol buffers**: Overkill, unfamiliar to Haskell ecosystem

### Serialization Formats
```haskell
data HandoffObject = HandoffObject
  { taskSummary :: Text
  , keyOutputs :: [Text]
  , metadata :: HandoffMetadata
  , customFields :: HashMap Text Value
  }

-- JSON for programmatic access
instance ToJSON HandoffObject
instance FromJSON HandoffObject

-- Structured text for prompt injection
toPromptText :: HandoffObject -> Text
toPromptText ho = T.unlines
  [ "=== Context from Previous Agent ==="
  , "Task: " <> taskSummary ho
  , "Key Outputs:"
  , T.unlines (map ("- " <>) (keyOutputs ho))
  , "Metadata: " <> renderMetadata (metadata ho)
  , "==================================="
  ]
```

---

## 8. Tool Timeout Implementation

### Decision
Use `timeout` function from `System.Timeout` with per-tool configurable duration (default 10s).

### Rationale
- **Standard library**: No external dependency
- **Simple**: `timeout microseconds action` returns `Maybe result`
- **Configurable**: Tool definition includes optional timeout override
- **Haskell-native**: Works with IO actions, composable

### Alternatives Considered
1. **Manual threading with timeout**: Complex, error-prone
2. **External process with kill**: Overkill for library
3. **No timeout**: Violates FR-010, FR-035

### Implementation
```haskell
data Tool = Tool
  { toolName :: Text
  , toolExecute :: ToolInput -> IO ToolOutput
  , toolTimeout :: Maybe Int  -- Microseconds; Nothing = default 10s
  , ...
  }

executeTool :: Tool -> ToolInput -> IO (Either ToolError ToolOutput)
executeTool tool input = do
  let timeoutDuration = fromMaybe 10_000_000 (toolTimeout tool)
  result <- timeout timeoutDuration (toolExecute tool input)
  case result of
    Nothing -> return $ Left TimeoutError
    Just output -> return $ Right output
```

---

## 9. Circular Dependency Detection

### Decision
Track agent call stack in AgentContext; compare new call against stack before execution.

### Rationale
- **Simple**: O(n) check where n = call depth (≤5)
- **Explicit**: Error message shows full call chain
- **No external state**: Stack maintained in context, thread-safe

### Alternatives Considered
1. **Global registry**: Thread-unsafe, complex cleanup
2. **Static analysis**: Can't detect runtime dynamic agent selection
3. **No detection**: Violates FR-038, risks stack overflow

### Implementation
```haskell
data AgentContext = AgentContext
  { ...
  , callStack :: [AgentId]  -- Most recent first
  , maxDepth :: Int         -- Default 5 (FR-022)
  }

executeAgent :: Agent -> AgentContext -> Text -> IO AgentResult
executeAgent agent ctx input = do
  -- Check circular dependency
  when (agentId agent `elem` callStack ctx) $
    throwIO $ CircularDependencyError (agentId agent) (callStack ctx)

  -- Check depth
  when (length (callStack ctx) >= maxDepth ctx) $
    throwIO $ MaxDepthExceededError (maxDepth ctx)

  -- Push to stack and execute
  let ctx' = ctx { callStack = agentId agent : callStack ctx }
  ...
```

---

## 10. Testing Strategy

### Decision
Three-tier testing: unit tests (Hspec), property-based tests (QuickCheck), integration tests (fixture data).

### Rationale
- **Unit tests**: Fast feedback, module-level correctness
- **Property-based**: Token counting accuracy, summarization preservation
- **Integration**: End-to-end workflows with mock LLM responses (no live API calls)
- **Constitution compliance**: QuickCheck mandated (IV. Test-Driven Quality)

### Test Categories
1. **Unit (Hspec)**:
   - Tool execution and timeout handling
   - Skill loading and lazy content retrieval
   - HandoffObject serialization
   - Logging output formatting

2. **Property-based (QuickCheck)**:
   - Token counting within 5% of reference (SC-011)
   - Context summarization reduces tokens by ≥50% (SC-012)
   - Circular dependency detection catches all cycles
   - Parallel execution produces same results regardless of order

3. **Integration**:
   - Full agent workflow with 3-5 tool calls
   - Sequential orchestration with handoffs
   - Context summarization trigger and recovery
   - Skill discovery and application

### Mock Strategy
```haskell
-- Fixtures for integration tests
data MockLLM = MockLLM
  { responses :: [Text]  -- Pre-scripted responses
  , toolCalls :: IORef [(Text, ToolInput)]  -- Captured calls
  }

-- No live LLM calls in CI
runIntegrationTest :: IO ()
runIntegrationTest = do
  mockLLM <- createMockLLM [response1, response2, ...]
  agent <- createAgent mockLLM tools
  result <- executeAgent agent "test input"
  assertEqual expected result
```

---

## Summary of Key Architectural Decisions

| Area | Decision | Primary Rationale |
|------|----------|------------------|
| Tokenization | tiktoken-rs via FFI | 5% accuracy requirement, production-proven |
| LLM Integration | Wrap langchain-hs | Leverage existing work, focus on orchestration |
| Context Summarization | Self-summarization via LLM | Context-aware, no external models |
| Skill Format | YAML front-matter + markdown | Human-readable, lazy-loadable |
| Parallel Execution | Haskell `async` library | Native concurrency, I/O-bound workload |
| Logging | monad-logger + custom handlers | Ecosystem integration + flexibility |
| Handoff Serialization | JSON + structured text | Programmatic access + prompt injection |
| Tool Timeout | `System.Timeout` | Standard library, simple, composable |
| Circular Detection | Call stack in context | Simple, explicit errors, thread-safe |
| Testing | Hspec + QuickCheck + integration | Coverage across unit/property/e2e |

All decisions align with Constitutional requirements and functional requirements from spec.md.
