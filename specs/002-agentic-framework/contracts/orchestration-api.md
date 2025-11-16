# Orchestration API Contract

**Module**: `AgenticFramework.Orchestration`
**Purpose**: Multi-agent workflows (sequential, parallel, agent-as-tool)
**Version**: 1.0.0

---

## Sequential Workflows

### executeSequential

**Signature**:
```haskell
executeSequential :: [Agent] -> Text -> IO [AgentResult]
```

**Description**: Executes agents in order, passing HandoffObject between them.

**Behavior** (FR-017, FR-023):
1. Execute first agent with input
2. Create HandoffObject from result (task summary, key outputs, metadata)
3. Pass HandoffObject to next agent via structured prompt injection
4. Repeat until all agents complete or error occurs

**Error Handling** (FR-036):
- Stops workflow on first unrecoverable error
- Returns results up to failure point
- Final result contains error context

**Example**:
```haskell
let analyzer = createAgent analyzerConfig
    generator = createAgent generatorConfig
    reviewer = createAgent reviewerConfig

results <- executeSequential [analyzer, generator, reviewer] "Build a login page"
-- Analyzer -> Generator -> Reviewer, each building on previous
```

**Fulfills**: FR-017, FR-023, FR-036

---

### executeSequentialWith

**Signature**:
```haskell
executeSequentialWith :: SequentialConfig -> [Agent] -> Text -> IO [AgentResult]
```

**Description**: Sequential execution with custom handoff transformation.

**Configuration**:
```haskell
data SequentialConfig = SequentialConfig
  { seqHandoffTransform :: HandoffObject -> HandoffObject
  , seqErrorPolicy :: ErrorPolicy
  }

data ErrorPolicy = StopOnError | ContinueOnError
```

**Example**:
```haskell
let config = SequentialConfig
      { seqHandoffTransform = \ho -> ho { handoffCustomFields = ... }
      , seqErrorPolicy = StopOnError
      }
results <- executeSequentialWith config agents input
```

**Fulfills**: FR-023a (programmatic handoff modification)

---

## Parallel Workflows

### executeParallel

**Signature**:
```haskell
executeParallel :: [Agent] -> Text -> IO [AgentResult]
```

**Description**: Executes agents concurrently on same input, collects all results.

**Behavior** (FR-018, FR-024):
- Uses `async` library for concurrent execution
- Each agent runs independently
- Failures in one agent don't affect others
- Results collected as agents complete
- Returns results in agent order (not completion order)

**Example**:
```haskell
let coder1 = createAgent pythonConfig
    coder2 = createAgent haskellConfig
    coder3 = createAgent rustConfig

results <- executeParallel [coder1, coder2, coder3] "Implement FizzBuzz"
-- Three implementations in parallel
```

**Fulfills**: FR-018, FR-024, SC-006 (linear scaling)

---

### executeParallelWith

**Signature**:
```haskell
executeParallelWith :: ParallelConfig -> [Agent] -> Text -> IO [AgentResult]
```

**Configuration**:
```haskell
data ParallelConfig = ParallelConfig
  { parAggregation :: AggregationStrategy
  , parMaxConcurrent :: Maybe Int  -- Limit concurrency, Nothing = unlimited
  }

data AggregationStrategy
  = CollectAll                     -- Return all results
  | FirstSuccess                   -- Return first successful result
  | MajorityVote (AgentResult -> Text)  -- Extract answer for voting
```

---

## Agent-as-Tool Pattern

### createAgentTool

**Signature**:
```haskell
createAgentTool :: Agent -> Tool
```

**Description**: Wraps an agent as a tool that other agents can invoke.

**Behavior** (FR-020):
- Tool name: "delegate_to_{agentName}"
- Tool execution: Runs wrapped agent, returns its output
- Nested calls tracked in call stack (FR-021)
- Maximum depth enforced (FR-022: default 5 levels)

**Example**:
```haskell
let sqlExpert = createAgent sqlConfig
    pythonExpert = createAgent pythonConfig

let sqlTool = createAgentTool sqlExpert
    pythonTool = createAgentTool pythonExpert

let coordinator = createAgent coordinatorConfig
      { configTools = [sqlTool, pythonTool, ...] }

-- Coordinator can now delegate to specialists
result <- executeAgent coordinator "Analyze database and plot results"
```

**Fulfills**: FR-020 (agent-as-tool), FR-021 (circular prevention), FR-022 (depth limit)

---

## HandoffObject Management

### createHandoff

**Signature**:
```haskell
createHandoff :: AgentResult -> HandoffObject
```

**Description**: Creates HandoffObject from agent result (auto-called in sequential flows).

**Extraction**:
- `taskSummary`: Extracts from agent's final output (first 200 chars)
- `keyOutputs`: Tool outputs with high importance (files created, search results)
- `metadata`: Source agent ID, timestamp, execution status, tools used

---

### modifyHandoff

**Signature**:
```haskell
modifyHandoff :: (HandoffObject -> HandoffObject) -> SequentialConfig -> SequentialConfig
```

**Description**: Adds handoff transformation to sequential configuration.

**Example**:
```haskell
let config = defaultSeqConfig
      & modifyHandoff (\ho -> ho
          { handoffCustomFields = HM.insert "priority" (A.String "high") (handoffCustomFields ho)
          })
```

---

### handoffToPrompt

**Signature**:
```haskell
handoffToPrompt :: HandoffObject -> Text
```

**Description**: Converts HandoffObject to structured text for prompt injection.

**Format** (FR-023b):
```text
=== Context from Previous Agent ===
Task: {taskSummary}
Key Outputs:
- {output1}
- {output2}
Metadata: Agent {agentId} completed at {timestamp} with status {status}
===================================
```

---

## Error Handling

### CircularDependencyError

**Raised When**: Agent calls another agent that's already in the call stack.

**Example**:
```haskell
-- Agent A has Agent B as tool
-- Agent B has Agent A as tool
-- A calls B calls A -> CircularDependencyError
```

**Prevention**: Call stack checked before each agent-as-tool invocation (FR-038).

---

### MaxDepthExceededError

**Raised When**: Nested agent calls exceed maximum depth (default 5).

**Example**:
```haskell
-- A -> B -> C -> D -> E -> F (depth 6) -> MaxDepthExceededError
```

**Configuration**:
```haskell
let agent = createAgent config { configMaxDepth = Just 10 }  -- Allow deeper nesting
```

---

## Performance Characteristics

- **Sequential**: O(n * agent_latency) where n = number of agents
  - 3 agents, 5s each = ~15s total
- **Parallel**: O(max_agent_latency) with linear scaling up to 5 agents (SC-006)
  - 3 agents, 5s each = ~5s total (3x speedup)
- **Agent-as-Tool**: Adds ~100ms overhead per nested call (call stack management)

---

## Thread Safety

- All orchestration functions create independent execution contexts
- Parallel execution uses thread-safe `async` primitives
- HandoffObject is immutable and thread-safe

---

**See Also**:
- `agent-api.md` - Agent execution basics
- `data-model.md` - HandoffObject structure details
- `observability-api.md` - Logging orchestration events
