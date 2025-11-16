# Agent API Contract

**Module**: `AgenticFramework.Agent`
**Purpose**: Agent creation, configuration, and execution
**Version**: 1.0.0

---

## Core Functions

### createAgent

**Signature**:
```haskell
createAgent :: AgentConfig -> IO Agent
```

**Description**: Creates a new agent with specified configuration.

**Parameters**:
- `AgentConfig`:
  ```haskell
  data AgentConfig = AgentConfig
    { configName :: Text
    , configSystemPrompt :: Text
    , configTools :: [Tool]
    , configLLM :: LLMConfig
    , configSkillsDir :: Maybe FilePath
    , configMaxTokens :: Maybe Int
    , configTemperature :: Maybe Double
    }
  ```

**Returns**: `Agent` instance ready for execution

**Errors**:
- `InvalidConfigError Text` - if system prompt is empty or temperature out of range
- `LLMConnectionError Text` - if LLM provider unreachable

**Example**:
```haskell
import AgenticFramework.Agent

main :: IO ()
main = do
  let config = AgentConfig
        { configName = "assistant"
        , configSystemPrompt = "You are a helpful assistant."
        , configTools = [readFileTool, calculatorTool]
        , configLLM = openAIConfig "gpt-4"
        , configSkillsDir = Just "./skills"
        , configMaxTokens = Nothing  -- Use model default
        , configTemperature = Just 0.7
        }
  agent <- createAgent config
  putStrLn $ "Created agent: " <> configName config
```

**Fulfills**: FR-001, FR-002

---

### executeAgent

**Signature**:
```haskell
executeAgent :: Agent -> Text -> IO AgentResult
```

**Description**: Executes agent on given input, returns result with context and logs.

**Parameters**:
- `Agent` - configured agent instance
- `Text` - input prompt/task for the agent

**Returns**: `AgentResult` containing:
  - `resultOutput :: Text` - agent's final response
  - `resultContext :: AgentContext` - final execution state
  - `resultLog :: ExecutionLog` - complete execution trace
  - `resultSuccess :: Bool` - whether execution succeeded

**Side Effects**:
- Calls LLM API (blocking I/O)
- Executes tools (file I/O, network requests)
- Writes logs (if file handler configured)
- May trigger context summarization at 90% token threshold

**Errors**:
- `LLMError Text` - LLM API failure
- `ToolError Text Text` - tool execution failure (tool name, error message)
- `MaxDepthExceeded Int` - nested agent calls exceed limit
- `CircularDependency AgentId [AgentId]` - circular agent call detected

**Example**:
```haskell
result <- executeAgent agent "What is 2 + 2?"
case resultSuccess result of
  True -> putStrLn $ "Agent says: " <> resultOutput result
  False -> putStrLn $ "Agent failed: " <> fromMaybe "Unknown error" (resultError result)
```

**Fulfills**: FR-005, FR-010, FR-033, FR-034, FR-035, FR-037, FR-038, FR-040, FR-041

---

### executeAgentWithContext

**Signature**:
```haskell
executeAgentWithContext :: Agent -> AgentContext -> Text -> IO AgentResult
```

**Description**: Executes agent with existing context (for continuing conversations).

**Parameters**:
- `Agent` - configured agent
- `AgentContext` - previous execution context (from prior `AgentResult`)
- `Text` - new input

**Returns**: `AgentResult` with updated context

**Use Case**: Multi-turn conversations where you want to preserve history.

**Example**:
```haskell
-- First interaction
result1 <- executeAgent agent "My name is Alice"

-- Continue conversation with context
let ctx = resultContext result1
result2 <- executeAgentWithContext agent ctx "What is my name?"
-- Agent remembers: "Your name is Alice"
```

**Fulfills**: FR-005, FR-039 (preserves conversation history)

---

### executeAgentStreaming

**Signature**:
```haskell
executeAgentStreaming :: Agent -> Text -> (Text -> IO ()) -> IO AgentResult
```

**Description**: Executes agent with streaming output callback (future enhancement - Phase 2+).

**Status**: OUT OF SCOPE for initial release (per spec.md "Real-time Streaming")

---

## Configuration Builders

### defaultAgentConfig

**Signature**:
```haskell
defaultAgentConfig :: Text -> Text -> LLMConfig -> AgentConfig
defaultAgentConfig name systemPrompt llm = AgentConfig
  { configName = name
  , configSystemPrompt = systemPrompt
  , configTools = []
  , configLLM = llm
  , configSkillsDir = Nothing
  , configMaxTokens = Nothing
  , configTemperature = Just 0.7
  }
```

**Description**: Sensible defaults for agent configuration.

---

### addTool

**Signature**:
```haskell
addTool :: Tool -> AgentConfig -> AgentConfig
addTool tool config = config { configTools = tool : configTools config }
```

**Description**: Adds a tool to agent configuration.

**Example**:
```haskell
let config = defaultAgentConfig "coder" "You write code" ollamaConfig
            & addTool readFileTool
            & addTool writeFileTool
```

---

## Read-Only Accessors

### getAgentId

**Signature**:
```haskell
getAgentId :: Agent -> AgentId
```

**Description**: Returns unique identifier for agent.

---

### getAvailableTools

**Signature**:
```haskell
getAvailableTools :: Agent -> [Tool]
```

**Description**: Returns list of tools the agent can access.

**Fulfills**: FR-003 (tool authorization visibility)

---

### getSystemPrompt

**Signature**:
```haskell
getSystemPrompt :: Agent -> Text
```

**Description**: Returns agent's system prompt.

---

## Invariants

1. **Unique Agent IDs**: Each `createAgent` call generates a unique UUID
2. **Non-empty System Prompt**: `configSystemPrompt` must contain at least one character
3. **Valid Temperature**: If specified, must be in range [0.0, 2.0]
4. **Tool Authorization**: Agents can only execute tools in their `availableTools` list (FR-004)
5. **Context Preservation**: `executeAgentWithContext` preserves all prior messages and tool history

---

## Error Handling

All errors are raised as exceptions in IO. Recommended pattern:

```haskell
import Control.Exception (catch, SomeException)

executeAgentSafe :: Agent -> Text -> IO (Either Text AgentResult)
executeAgentSafe agent input = do
  result <- catch
    (Right <$> executeAgent agent input)
    (\(e :: SomeException) -> return $ Left (show e))
  return result
```

---

## Performance Characteristics

- **Agent Creation**: O(1), <10ms typical
- **Agent Execution**: O(n) where n = number of tool calls + LLM calls
  - Single LLM call: 500ms - 5s (network + model latency)
  - Tool execution: varies by tool, subject to timeout (default 10s per FR-010)
  - Context summarization (if triggered): 1-5s additional

---

## Thread Safety

- `Agent` instances are immutable and thread-safe
- `executeAgent` can be called concurrently on same agent (creates independent contexts)
- `AgentContext` is not thread-safe - do not share across threads

---

**See Also**:
- `tool-api.md` - Tool creation and management
- `observability-api.md` - Logging and metrics access
- `orchestration-api.md` - Multi-agent workflows
