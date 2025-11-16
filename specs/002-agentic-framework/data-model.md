# Data Model: Agentic Framework Library

**Date**: 2025-11-15
**Feature**: 002-agentic-framework
**Purpose**: Define concrete Haskell types for all entities from spec.md

---

## Core Entities

### 1. Agent

**Description**: Autonomous reasoning entity with system prompt, tools, configuration, and execution state.

**Haskell Type**:
```haskell
data Agent = Agent
  { agentId :: AgentId
  , agentName :: Text
  , systemPrompt :: Text
  , availableTools :: [Tool]
  , llmConfig :: LLMConfig
  , skillsDirectory :: Maybe FilePath
  , maxTokens :: Maybe Int
  , temperature :: Maybe Double
  , contextThreshold :: Double  -- Default 0.9 (90%)
  } deriving (Show, Eq)

newtype AgentId = AgentId { unAgentId :: UUID }
  deriving (Show, Eq, Ord, ToJSON, FromJSON)
```

**Validation Rules** (from FR-001, FR-002, FR-003):
- `agentId` must be unique within execution context
- `systemPrompt` must be non-empty
- `availableTools` contains only authorized tools for this agent
- `temperature` must be in range [0.0, 2.0] if specified
- `contextThreshold` must be in range (0.0, 1.0], default 0.9

**Relationships**:
- Has many Tools (composition)
- Creates AgentContext during execution
- Produces AgentResult when executed

---

### 2. Tool

**Description**: Capability that agents can invoke with defined I/O schema and execution function.

**Haskell Type**:
```haskell
data Tool = Tool
  { toolName :: Text
  , toolDescription :: Text
  , toolSchema :: ToolSchema
  , toolExecute :: ToolInput -> IO (Either ToolError ToolOutput)
  , toolTimeout :: Maybe Int  -- Microseconds; Nothing = 10s default
  , toolRetryable :: Bool     -- Default True (supports 3 retries per FR-037)
  } deriving (Generic)

data ToolSchema = ToolSchema
  { inputSchema :: Value    -- JSON Schema for input validation
  , outputSchema :: Value   -- JSON Schema for output validation
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ToolInput = ToolInput { unToolInput :: HashMap Text Value }
  deriving (Show, Eq, ToJSON, FromJSON)

newtype ToolOutput = ToolOutput { unToolOutput :: HashMap Text Value }
  deriving (Show, Eq, ToJSON, FromJSON)

data ToolError
  = ToolExecutionError Text
  | ToolTimeoutError
  | ToolValidationError Text
  | ToolAuthorizationError
  deriving (Show, Eq)
```

**Validation Rules** (from FR-009, FR-010):
- `toolName` must be unique within an agent's available tools
- `toolSchema` must be valid JSON Schema
- `toolTimeout` must be positive if specified
- Input/output must conform to schemas

**Built-in Tools** (from FR-006, FR-007, FR-008):
- File operations: `readFile`, `writeFile`, `listDirectory`, `getFileInfo`
- Web search: `webSearch` (requires API key configuration)
- LangChain wrappers: `calculator`, `wikipedia`, `webScraper`

---

### 3. Skill

**Description**: Reusable methodology document with label, description, category, and content.

**Haskell Type**:
```haskell
data Skill = Skill
  { skillLabel :: SkillLabel
  , skillDescription :: Text
  , skillCategory :: Maybe SkillCategory
  , skillContent :: SkillContent  -- Lazy loaded
  } deriving (Show, Eq)

newtype SkillLabel = SkillLabel { unSkillLabel :: Text }
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype SkillCategory = SkillCategory { unSkillCategory :: Text }
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data SkillContent
  = NotLoaded                     -- Initial state
  | Loaded SkillMarkdown          -- Full content loaded
  deriving (Show, Eq)

data SkillMarkdown = SkillMarkdown
  { purpose :: Maybe Text
  , methodology :: Maybe Text
  , examples :: Maybe Text
  , rawMarkdown :: Text           -- Full content for fallback
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

**Validation Rules** (from FR-012a, FR-012b):
- `skillLabel` must be unique within skills directory
- `skillDescription` must be non-empty (required for discovery)
- Content loaded lazily only when requested (FR-012b)
- File format: YAML front-matter + markdown body

**Lifecycle** (from FR-016):
- Initial load: Parse headers only (label, description, category)
- On-demand: Parse full markdown content when agent requests skill
- Hot reload: Watch file system, reload on change without restart

---

### 4. AgentContext

**Description**: Execution state and environment for an agent including conversation, tools, skills, and token metrics.

**Haskell Type**:
```haskell
data AgentContext = AgentContext
  { contextAgent :: Agent
  , contextConversation :: [Message]
  , contextToolHistory :: [ToolExecution]
  , contextLoadedSkills :: HashMap SkillLabel Skill
  , contextTokenMetrics :: TokenMetrics
  , contextOrchestration :: Maybe OrchestrationContext
  , contextCallStack :: [AgentId]         -- For circular detection
  , contextMaxDepth :: Int                -- Default 5 (FR-022)
  } deriving (Show, Eq)

data Message
  = UserMessage { messageContent :: Text, messageTimestamp :: UTCTime }
  | AssistantMessage { messageContent :: Text, messageTimestamp :: UTCTime }
  | SystemMessage { messageContent :: Text, messageTimestamp :: UTCTime }
  | ToolMessage { messageTool :: Text, messageResult :: ToolOutput, messageTimestamp :: UTCTime }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ToolExecution = ToolExecution
  { toolExecName :: Text
  , toolExecInput :: ToolInput
  , toolExecOutput :: Either ToolError ToolOutput
  , toolExecTimestamp :: UTCTime
  , toolExecDuration :: NominalDiffTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

**State Transitions**:
1. **Created**: New context with empty conversation, no skills loaded
2. **Executing**: Messages being added, tools being called
3. **Summarizing**: Token threshold reached, context being condensed
4. **Completed**: Final result produced, context frozen

**Invariants**:
- `length contextCallStack ≤ contextMaxDepth`
- `contextTokenMetrics.currentTokenCount` updated after each message
- Summarization triggered when `percentageUsed ≥ contextThreshold`

---

### 5. TokenMetrics

**Description**: Real-time token usage information for context window management.

**Haskell Type**:
```haskell
data TokenMetrics = TokenMetrics
  { currentTokenCount :: Int
  , modelTokenLimit :: Int
  , percentageUsed :: Double       -- 0.0 to 100.0
  , summarizationTriggered :: Bool
  , lastSummarization :: Maybe UTCTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Read-only API for developers (FR-002b)
getTokenMetrics :: AgentContext -> TokenMetrics
getTokenMetrics = contextTokenMetrics

-- Internal update function (not exported)
updateTokenMetrics :: AgentContext -> Text -> AgentContext
updateTokenMetrics ctx newText = ctx
  { contextTokenMetrics = recalculateMetrics ctx newText }
```

**Validation Rules** (from FR-039, FR-040, FR-041):
- `currentTokenCount` accurate within 5% of actual model count (SC-011)
- `percentageUsed = (currentTokenCount / modelTokenLimit) * 100`
- Summarization triggers when `percentageUsed ≥ contextThreshold` (default 90%)
- `modelTokenLimit` determined by LLM model (e.g., 8192 for GPT-3.5, 128000 for GPT-4)

---

### 6. HandoffObject

**Description**: Structured context passed between agents in orchestration workflows.

**Haskell Type**:
```haskell
data HandoffObject = HandoffObject
  { handoffTaskSummary :: Text
  , handoffKeyOutputs :: [Text]
  , handoffMetadata :: HandoffMetadata
  , handoffCustomFields :: HashMap Text Value  -- Extensible
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data HandoffMetadata = HandoffMetadata
  { sourceAgentId :: AgentId
  , handoffTimestamp :: UTCTime
  , executionStatus :: ExecutionStatus
  , toolsUsed :: [Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ExecutionStatus
  = Success
  | PartialSuccess Text  -- Warning message
  | Failed Text          -- Error message
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

**Serialization** (from FR-023b):
```haskell
-- JSON for programmatic access
instance ToJSON HandoffObject
instance FromJSON HandoffObject

-- Structured text for prompt injection
toPromptText :: HandoffObject -> Text
toPromptText ho = ...  -- See research.md §7
```

**Validation Rules** (from FR-023):
- `handoffTaskSummary` must be non-empty
- `sourceAgentId` must be valid agent in workflow
- Developers can modify via `modifyHandoff :: (HandoffObject -> HandoffObject) -> Workflow -> Workflow`

---

### 7. Orchestration

**Description**: Workflow pattern coordinating multiple agents.

**Haskell Type**:
```haskell
data Orchestration
  = Sequential SequentialWorkflow
  | Parallel ParallelWorkflow
  | AgentAsTool AgentToolWorkflow
  deriving (Show, Eq)

data SequentialWorkflow = SequentialWorkflow
  { seqAgents :: [Agent]
  , seqHandoffTransform :: Maybe (HandoffObject -> HandoffObject)
  , seqErrorPolicy :: ErrorPolicy
  } deriving (Show, Eq)

data ParallelWorkflow = ParallelWorkflow
  { parAgents :: [Agent]
  , parInput :: Text               -- Same input to all or different?
  , parAggregation :: AggregationStrategy
  } deriving (Show, Eq)

data AgentToolWorkflow = AgentToolWorkflow
  { coordAgent :: Agent
  , expertAgents :: HashMap Text Agent  -- Keyed by specialty
  } deriving (Show, Eq)

data ErrorPolicy
  = StopOnError                    -- FR-036: Stop sequential workflow
  | ContinueOnError                -- Log error, continue to next agent
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AggregationStrategy
  = CollectAll                     -- Return all results
  | MajorityVote                   -- For consensus tasks
  | FirstSuccess                   -- Return first successful result
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

**Validation Rules** (from FR-017, FR-018, FR-020, FR-021):
- `seqAgents` must be non-empty
- No circular dependencies in `coordAgent` calling `expertAgents`
- Maximum nesting depth enforced via call stack
- Each agent in workflow must have unique ID

---

### 8. ExecutionLog

**Description**: Record of agent activity for observability.

**Haskell Type**:
```haskell
data ExecutionLog = ExecutionLog
  { logId :: UUID
  , logAgentId :: AgentId
  , logEntries :: Seq LogEntry
  , logStartTime :: UTCTime
  , logEndTime :: Maybe UTCTime
  } deriving (Show, Eq)

data LogEntry = LogEntry
  { logLevel :: LogLevel
  , logTimestamp :: UTCTime
  , logAgentId :: Maybe AgentId
  , logToolName :: Maybe Text
  , logMessage :: Text
  , logMetadata :: HashMap Text Value
  , logCallStack :: [AgentId]       -- For nested agent calls
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LogLevel
  = DEBUG
  | INFO
  | WARN
  | ERROR
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)
```

**Filtering API** (from FR-029):
```haskell
filterLogs :: ExecutionLog -> LogFilter -> [LogEntry]
filterLogs log f = toList $ Seq.filter (matchesFilter f) (logEntries log)

data LogFilter = LogFilter
  { filterAgentId :: Maybe AgentId
  , filterToolName :: Maybe Text
  , filterMinLevel :: LogLevel
  , filterTimeRange :: Maybe (UTCTime, UTCTime)
  , filterHasError :: Bool
  } deriving (Show, Eq)
```

**Export Formats** (from FR-032):
- **JSON**: Via `ToJSON ExecutionLog` instance
- **Structured text**: Human-readable format with colorization

---

### 9. LLMConfig

**Description**: Configuration for LLM provider integration via langchain-hs.

**Haskell Type**:
```haskell
data LLMConfig = LLMConfig
  { llmProvider :: LLMProvider
  , llmModel :: Text               -- e.g., "gpt-4", "claude-3-opus"
  , llmApiKey :: Maybe Text        -- Retrieved from env if Nothing
  , llmBaseUrl :: Maybe Text       -- For custom endpoints
  , llmMaxTokens :: Int            -- Model's context window size
  , llmTemperature :: Double       -- Default 0.7
  } deriving (Show, Eq)

data LLMProvider
  = Ollama
  | OpenAI
  | Claude
  | HuggingFace
  | Custom Text                    -- Custom provider name
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

**Validation** (from A-003):
- API keys managed by developer (not library)
- `llmMaxTokens` must match actual model limit for accurate token management
- Provider-specific defaults: OpenAI (gpt-3.5-turbo, 4096 tokens), Claude (claude-3-sonnet, 200000 tokens)

---

### 10. AgentResult

**Description**: Output from agent execution.

**Haskell Type**:
```haskell
data AgentResult = AgentResult
  { resultOutput :: Text
  , resultContext :: AgentContext          -- Final state
  , resultLog :: ExecutionLog
  , resultToolsUsed :: [ToolExecution]
  , resultSuccess :: Bool
  , resultError :: Maybe Text
  } deriving (Show, Eq)

-- Success constructor
successResult :: Text -> AgentContext -> ExecutionLog -> AgentResult
successResult output ctx log = AgentResult
  { resultOutput = output
  , resultContext = ctx
  , resultLog = log
  , resultToolsUsed = contextToolHistory ctx
  , resultSuccess = True
  , resultError = Nothing
  }

-- Failure constructor
failureResult :: Text -> AgentContext -> ExecutionLog -> AgentResult
failureResult err ctx log = AgentResult
  { resultOutput = ""
  , resultContext = ctx
  , resultLog = log
  , resultToolsUsed = contextToolHistory ctx
  , resultSuccess = False
  , resultError = Just err
  }
```

---

## Relationships Diagram

```text
┌─────────┐       creates        ┌──────────────┐
│  Agent  │──────────────────────>│ AgentContext │
└─────────┘                       └──────────────┘
     │                                    │
     │ has many                           │ contains
     v                                    v
┌─────────┐                       ┌──────────────┐
│  Tool   │                       │ TokenMetrics │
└─────────┘                       └──────────────┘
                                          │
                                          │ tracks
     ┌────────────────────────────────────v
     │                            ┌──────────────┐
     │                            │   Message    │
     │                            └──────────────┘
     │                                    │
     │                                    │ logged in
     v                                    v
┌─────────┐                       ┌──────────────┐
│  Skill  │<───── loaded in ──────│ExecutionLog  │
└─────────┘       AgentContext    └──────────────┘

┌──────────────┐     coordinates    ┌─────────┐
│Orchestration │────────────────────>│  Agent  │
└──────────────┘       multiple      └─────────┘
     │                                    │
     │ uses                               │ produces
     v                                    v
┌──────────────┐                   ┌──────────────┐
│HandoffObject │                   │ AgentResult  │
└──────────────┘                   └──────────────┘
```

---

## Validation Summary

| Entity | Key Constraints |
|--------|----------------|
| Agent | Unique ID, non-empty system prompt, valid tools, temperature ∈ [0, 2] |
| Tool | Unique name per agent, valid JSON schemas, positive timeout |
| Skill | Unique label, non-empty description, lazy content loading |
| AgentContext | Call stack depth ≤ max, token metrics current, no circular deps |
| TokenMetrics | Count accurate within 5%, percentage = count/limit * 100 |
| HandoffObject | Non-empty task summary, valid source agent ID |
| Orchestration | Non-empty agent list, no circular dependencies |
| ExecutionLog | Chronological entries, filterable by agent/tool/level/time |
| LLMConfig | Valid provider, model matches token limit |
| AgentResult | Success XOR error populated |

---

All types align with functional requirements (FR-001 through FR-044) and key entities defined in spec.md.
