# Observability API Contract

**Module**: `AgenticFramework.Logging`
**Purpose**: Logging, metrics, and execution tracing
**Version**: 1.0.0

---

## Logging

### runWithLogging

**Signature**:
```haskell
runWithLogging :: LogConfig -> (forall m. MonadLogger m => m a) -> IO a
```

**Description**: Runs agent execution with logging configuration.

**Configuration**:
```haskell
data LogConfig = LogConfig
  { logLevel :: LogLevel
  , logHandlers :: [LogHandler]
  , logColorize :: Bool            -- For stdout handler
  }

data LogLevel = DEBUG | INFO | WARN | ERROR
  deriving (Show, Eq, Ord)
```

**Default Handlers** (FR-029a, FR-029c, FR-032):
- `colorizedStdoutHandler` - Colored terminal output
- `fileHandler FilePath` - File-based logging with rotation
- `monadLoggerHandler` - Integrates with monad-logger ecosystem

**Example**:
```haskell
let config = LogConfig
      { logLevel = INFO
      , logHandlers = [colorizedStdoutHandler, fileHandler "./agent.log"]
      , logColorize = True
      }

runWithLogging config $ do
  agent <- liftIO $ createAgent agentConfig
  liftIO $ executeAgent agent "Hello, world!"
```

**Fulfills**: FR-029a (monad-logger integration), FR-029c (colorized output), FR-031 (log levels), FR-032 (handlers)

---

### Custom Log Handlers

### Creating a Custom Handler (FR-029b)

**Interface**:
```haskell
class LogHandler h where
  logEntry :: h -> LogEntry -> IO ()
  flushLog :: h -> IO ()  -- Called on shutdown

data LogEntry = LogEntry
  { logLevel :: LogLevel
  , logTimestamp :: UTCTime
  , logAgentId :: Maybe AgentId
  , logToolName :: Maybe Text
  , logMessage :: Text
  , logMetadata :: HashMap Text Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

**Example - Webhook Handler**:
```haskell
data WebhookHandler = WebhookHandler { webhookUrl :: Text }

instance LogHandler WebhookHandler where
  logEntry h entry = do
    let payload = A.encode entry
    void $ HTTP.post (T.unpack $ webhookUrl h) payload

  flushLog _ = return ()

-- Usage
let webhookHandler = WebhookHandler "https://logs.example.com/ingest"
let config = LogConfig INFO [webhookHandler] False
```

**Fulfills**: FR-029b (pluggable custom handlers)

---

## Token Metrics

### getTokenMetrics

**Signature**:
```haskell
getTokenMetrics :: AgentContext -> TokenMetrics
```

**Description**: Returns current token usage metrics (read-only).

**Returns**:
```haskell
data TokenMetrics = TokenMetrics
  { currentTokenCount :: Int
  , modelTokenLimit :: Int
  , percentageUsed :: Double         -- 0.0 to 100.0
  , summarizationTriggered :: Bool
  , lastSummarization :: Maybe UTCTime
  }
```

**Example**:
```haskell
result <- executeAgent agent "Long task..."
let metrics = getTokenMetrics (resultContext result)
putStrLn $ "Used " <> show (percentageUsed metrics) <> "% of context window"

when (summarizationTriggered metrics) $
  putStrLn "Context was summarized during execution"
```

**Fulfills**: FR-002b (read-only metrics API), SC-011 (5% accuracy)

---

## Execution Logs

### getExecutionLog

**Signature**:
```haskell
getExecutionLog :: AgentResult -> ExecutionLog
```

**Description**: Extracts complete execution log from agent result.

---

### filterLogs

**Signature**:
```haskell
filterLogs :: ExecutionLog -> LogFilter -> [LogEntry]
```

**Filters**:
```haskell
data LogFilter = LogFilter
  { filterAgentId :: Maybe AgentId
  , filterToolName :: Maybe Text
  , filterMinLevel :: LogLevel
  , filterTimeRange :: Maybe (UTCTime, UTCTime)
  , filterHasError :: Bool          -- Only entries with errors
  }

-- Helper constructors
allLogs :: LogFilter
toolCallsOnly :: Text -> LogFilter
errorsOnly :: LogFilter
agentOnly :: AgentId -> LogFilter
```

**Example**:
```haskell
let log = getExecutionLog result
    toolCalls = filterLogs log (toolCallsOnly "read_file")
    errors = filterLogs log errorsOnly

forM_ errors $ \entry ->
  putStrLn $ "Error at " <> show (logTimestamp entry) <> ": " <> logMessage entry
```

**Fulfills**: FR-029 (filtering by agent/tool/time/status), SC-010 (locate actions in <2min)

---

## Log Content

### What Gets Logged (FR-025 - FR-030)

**Always Logged**:
- Tool invocations: name, input, output, timestamp, duration (FR-025)
- Agent reasoning: internal thought processes from LLM (FR-026)
- Agent decisions: which tool selected and why (FR-027)
- Orchestration events: agent start/stop, handoffs, workflow state (FR-028)
- Errors: full context, stack trace, agent state (FR-030)
- Retry attempts: reasoning and parameter changes (FR-037a)
- Context summarization: trigger reason, token counts, what was summarized (FR-042)

**Log Entry Format**:
```json
{
  "level": "INFO",
  "timestamp": "2025-11-15T10:30:00Z",
  "agentId": "agent-123",
  "toolName": "read_file",
  "message": "Tool execution completed",
  "metadata": {
    "input": {"path": "/tmp/data.txt"},
    "output": {"content": "...", "size": 1024},
    "duration_ms": 15
  }
}
```

---

## Export Formats

### exportLogsJSON

**Signature**:
```haskell
exportLogsJSON :: ExecutionLog -> FilePath -> IO ()
```

**Description**: Exports logs as JSON array (one entry per line for streaming).

**Format**:
```json
{"level":"INFO","timestamp":"2025-11-15T10:30:00Z",...}
{"level":"DEBUG","timestamp":"2025-11-15T10:30:01Z",...}
```

---

### exportLogsText

**Signature**:
```haskell
exportLogsText :: ExecutionLog -> FilePath -> IO ()
```

**Description**: Exports logs as human-readable structured text.

**Format**:
```text
[2025-11-15 10:30:00 INFO] Agent agent-123: Starting execution
[2025-11-15 10:30:01 DEBUG] Agent agent-123: Selected tool: read_file
[2025-11-15 10:30:01 INFO] Tool read_file: Execution completed in 15ms
```

**Fulfills**: FR-032 (export formats)

---

## Colorized Output

### Color Scheme (FR-029c)

- **DEBUG**: Gray
- **INFO**: White
- **WARN**: Yellow
- **ERROR**: Red
- **Agent IDs**: Cyan
- **Tool Names**: Green
- **Timestamps**: Blue

**Example Output**:
```text
[10:30:00] INFO  [agent-123] Starting execution
[10:30:01] DEBUG [agent-123] Selected tool: read_file
[10:30:01] INFO  [agent-123] Tool read_file: Completed
[10:30:02] WARN  [agent-123] Context at 92% capacity, summarizing...
```

**Disable**: Set `logColorize = False` in LogConfig for non-terminal outputs.

---

## Performance Impact

- **Logging Overhead**: <5% of total execution time (SC-004: 100% capture)
- **Token Metrics**: <1ms per query (read from in-memory context)
- **Log Filtering**: O(n) where n = log entries, <10ms for 1000 entries

---

## Thread Safety

- All log handlers must be thread-safe (concurrent agent execution)
- Token metrics are immutable snapshots (no mutation)
- ExecutionLog uses `Seq` for O(1) append, safe for concurrent reads after completion

---

**See Also**:
- `agent-api.md` - AgentResult structure
- `data-model.md` - ExecutionLog and TokenMetrics details
- `orchestration-api.md` - Multi-agent logging
