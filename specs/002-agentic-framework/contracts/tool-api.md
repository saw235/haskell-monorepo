# Tool API Contract

**Module**: `AgenticFramework.Tool`
**Purpose**: Tool creation, built-in tools, and custom tool interface
**Version**: 1.0.0

---

## Core Functions

### createTool

**Signature**:
```haskell
createTool :: ToolConfig -> Tool
```

**Description**: Creates a custom tool with defined I/O schema and execution function.

**Parameters**:
```haskell
data ToolConfig = ToolConfig
  { toolConfigName :: Text
  , toolConfigDescription :: Text
  , toolConfigInputSchema :: Value        -- JSON Schema
  , toolConfigOutputSchema :: Value       -- JSON Schema
  , toolConfigExecute :: ToolInput -> IO (Either ToolError ToolOutput)
  , toolConfigTimeout :: Maybe Int        -- Microseconds, Nothing = 10s
  , toolConfigRetryable :: Bool           -- Default True
  }
```

**Returns**: `Tool` ready to be added to an agent

**Example**:
```haskell
import AgenticFramework.Tool
import qualified Data.Aeson as A

weatherTool :: Tool
weatherTool = createTool ToolConfig
  { toolConfigName = "get_weather"
  , toolConfigDescription = "Get current weather for a city"
  , toolConfigInputSchema = A.object
      [ "type" A..= ("object" :: Text)
      , "properties" A..= A.object
          [ "city" A..= A.object
              [ "type" A..= ("string" :: Text)
              , "description" A..= ("City name" :: Text)
              ]
          ]
      , "required" A..= (["city"] :: [Text])
      ]
  , toolConfigOutputSchema = A.object
      [ "type" A..= ("object" :: Text)
      , "properties" A..= A.object
          [ "temperature" A..= A.object ["type" A..= ("number" :: Text)]
          , "conditions" A..= A.object ["type" A..= ("string" :: Text)]
          ]
      ]
  , toolConfigExecute = \input -> do
      -- Fetch weather from API
      let city = input ^? key "city" . _String
      case city of
        Nothing -> return $ Left $ ToolValidationError "Missing city"
        Just c -> do
          temp <- fetchWeather c
          return $ Right $ ToolOutput $ HM.fromList
            [ ("temperature", A.Number temp)
            , ("conditions", A.String "sunny")
            ]
  , toolConfigTimeout = Just 5_000_000  -- 5 seconds
  , toolConfigRetryable = True
  }
```

**Fulfills**: FR-009

---

## Built-in Tools

### File Operations (FR-006)

#### readFileTool

**Signature**: `readFileTool :: Tool`

**Description**: Reads file content as text.

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "path": { "type": "string", "description": "File path to read" }
  },
  "required": ["path"]
}
```

**Output Schema**:
```json
{
  "type": "object",
  "properties": {
    "content": { "type": "string" },
    "size": { "type": "integer" }
  }
}
```

**Errors**:
- `ToolExecutionError "File not found: <path>"`
- `ToolExecutionError "Permission denied: <path>"`

---

#### writeFileTool

**Signature**: `writeFileTool :: Tool`

**Description**: Writes text content to file.

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "path": { "type": "string" },
    "content": { "type": "string" }
  },
  "required": ["path", "content"]
}
```

**Output Schema**:
```json
{
  "type": "object",
  "properties": {
    "bytesWritten": { "type": "integer" }
  }
}
```

---

#### listDirectoryTool

**Signature**: `listDirectoryTool :: Tool`

**Description**: Lists files and directories in a directory.

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "path": { "type": "string" },
    "recursive": { "type": "boolean", "default": false }
  },
  "required": ["path"]
}
```

**Output Schema**:
```json
{
  "type": "object",
  "properties": {
    "entries": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "name": { "type": "string" },
          "type": { "enum": ["file", "directory"] },
          "size": { "type": "integer" }
        }
      }
    }
  }
}
```

---

### Web Search (FR-007)

#### webSearchTool

**Signature**: `webSearchTool :: SearchConfig -> Tool`

**Description**: Performs web search using configured search API.

**Configuration**:
```haskell
data SearchConfig = SearchConfig
  { searchApiKey :: Text        -- SerpAPI, Google Custom Search, etc.
  , searchEngine :: SearchEngine
  , searchMaxResults :: Int      -- Default 5
  }

data SearchEngine = SerpAPI | GoogleCustomSearch | BingSearch
```

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "query": { "type": "string" },
    "num_results": { "type": "integer", "default": 5 }
  },
  "required": ["query"]
}
```

**Output Schema**:
```json
{
  "type": "object",
  "properties": {
    "results": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "title": { "type": "string" },
          "snippet": { "type": "string" },
          "url": { "type": "string" }
        }
      }
    }
  }
}
```

**Example**:
```haskell
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- getEnv "SERPAPI_KEY"
  let searchConfig = SearchConfig apiKey SerpAPI 5
      searchTool = webSearchTool searchConfig
  -- Add to agent tools...
```

**Requires**: External API key (per A-006)

---

### LangChain Wrappers (FR-008)

#### calculatorTool

**Signature**: `calculatorTool :: Tool`

**Description**: Evaluates mathematical expressions (wraps langchain-hs Calculator).

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "expression": { "type": "string", "description": "Math expression like '2 + 2'" }
  },
  "required": ["expression"]
}
```

**Output Schema**:
```json
{
  "type": "object",
  "properties": {
    "result": { "type": "number" }
  }
}
```

---

#### wikipediaTool

**Signature**: `wikipediaTool :: Tool`

**Description**: Searches Wikipedia and returns summary (wraps langchain-hs Wikipedia).

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "query": { "type": "string" }
  },
  "required": ["query"]
}
```

**Output Schema**:
```json
{
  "type": "object",
  "properties": {
    "summary": { "type": "string" },
    "url": { "type": "string" }
  }
}
```

---

#### webScraperTool

**Signature**: `webScraperTool :: Tool`

**Description**: Fetches and extracts text from web pages (wraps langchain-hs WebScraper).

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "url": { "type": "string", "format": "uri" },
    "selector": { "type": "string", "description": "CSS selector (optional)" }
  },
  "required": ["url"]
}
```

**Output Schema**:
```json
{
  "type": "object",
  "properties": {
    "text": { "type": "string" },
    "title": { "type": "string" }
  }
}
```

---

## Tool Execution

### executeTool (Internal)

**Signature**:
```haskell
executeTool :: Tool -> ToolInput -> IO (Either ToolError ToolOutput)
```

**Description**: Internal function to execute a tool with timeout and retry logic.

**Behavior** (from FR-010, FR-035, FR-037):
1. Validate input against `inputSchema`
2. Execute with timeout (default 10s, configurable per tool)
3. On timeout: Return `ToolTimeoutError`
4. On execution error: Return error, allow agent to retry up to 3 times total
5. On success: Validate output against `outputSchema`

**Not Exported**: Agents call tools implicitly via LLM reasoning; developers don't call this directly.

---

## Tool Validation

### validateToolInput

**Signature**:
```haskell
validateToolInput :: Tool -> ToolInput -> Either Text ()
```

**Description**: Validates tool input against JSON Schema.

**Returns**:
- `Right ()` if valid
- `Left errorMessage` if validation fails

---

### validateToolOutput

**Signature**:
```haskell
validateToolOutput :: Tool -> ToolOutput -> Either Text ()
```

**Description**: Validates tool output against JSON Schema.

---

## Error Types

```haskell
data ToolError
  = ToolExecutionError Text           -- Tool-specific error
  | ToolTimeoutError                  -- Exceeded timeout (FR-010)
  | ToolValidationError Text          -- Schema validation failed
  | ToolAuthorizationError            -- Tool not authorized for agent (FR-004)
  deriving (Show, Eq)
```

---

## Retry Policy

**Default Behavior** (FR-037):
- Tools marked `toolRetryable = True` can be retried up to 3 times
- Agent receives error message after each failure
- Agent can modify parameters and retry
- Total attempts logged (FR-037a)

**Example Retry Flow**:
```text
1. Agent calls readFileTool with path "/tmp/data.txt"
2. Tool fails: "File not found"
3. Agent receives error, reasons about alternative
4. Agent retries with path "/tmp/data.json"
5. Tool succeeds
```

---

## Custom Tool Best Practices

1. **Clear Descriptions**: Help LLM understand when to use the tool
2. **Strict Schemas**: Define required fields and types explicitly
3. **Error Messages**: Provide actionable error messages for retry
4. **Timeouts**: Set appropriate timeouts for I/O-bound operations
5. **Idempotency**: Make tools idempotent where possible (retries safer)

**Example**:
```haskell
-- Good: Specific, actionable error
return $ Left $ ToolExecutionError "Database connection failed. Check DB_URL environment variable."

-- Bad: Generic error
return $ Left $ ToolExecutionError "Error"
```

---

## Performance Characteristics

- **Tool Creation**: O(1), immediate
- **Tool Execution**: Varies by tool:
  - File I/O: 1-100ms
  - Web search: 200-2000ms (network latency)
  - Calculator: <1ms
- **Timeout Overhead**: <1ms (uses `System.Timeout`)

---

## Thread Safety

- `Tool` instances are immutable and thread-safe
- Tool execution functions must be thread-safe if agent uses parallel orchestration
- Consider using locks for tools modifying shared resources

---

**See Also**:
- `agent-api.md` - Adding tools to agents
- `observability-api.md` - Tool execution logging
