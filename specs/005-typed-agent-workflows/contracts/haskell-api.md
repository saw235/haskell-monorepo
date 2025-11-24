# Haskell API Contract: Typed Agent Workflows

**Version**: 1.0.0
**Module**: AgenticFramework.Workflow

## Core Types

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module AgenticFramework.Workflow.Types where

import Data.Text (Text)
import Data.Aeson (Value)
import Data.UUID (UUID)

-- | Agent with typed workflows and capabilities
data Agent caps = Agent
  { agentId           :: UUID
  , agentName         :: Text
  , agentSystemPrompt :: Text
  , agentTools        :: [Tool]
  , agentCapabilities :: [Capability]
  , agentWorkflow     :: Maybe (Workflow Value)
  , agentLLM          :: LLMConfig
  }

-- | Workflow monad with agent context access
newtype Workflow a = Workflow
  (ReaderT AgentContext (StateT WorkflowState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Context available during workflow execution
data AgentContext = AgentContext
  { ctxSystemPrompt   :: Text
  , ctxUserPrompt     :: Text
  , ctxTools          :: [Tool]
  , ctxCapabilities   :: [Capability]
  , ctxLLM            :: LLMConfig
  , ctxTokenCount     :: IORef Int
  , ctxHistory        :: IORef [Message]
  }

-- | Mutable workflow state
data WorkflowState = WorkflowState
  { wsStepResults     :: Map StepId Value
  , wsExecutionLog    :: [LogEntry]
  , wsCurrentPhase    :: WorkflowPhase
  }

-- | Workflow execution phases (for indexed monad variant)
data WorkflowPhase = Init | Configured | Ready | Executing | Complete

-- | Natural language capability
data Capability = Capability
  { capName           :: Text
  , capDescription    :: Text
  , capModifier       :: Prompt -> Prompt
  , capSource         :: CapabilitySource
  }

data CapabilitySource = Builtin | File FilePath | Dynamic

-- | Workflow step types
data WorkflowStep where
  LLMCall      :: Text -> WorkflowStep
  ToolUse      :: ToolName -> Value -> WorkflowStep
  SubWorkflow  :: Workflow a -> WorkflowStep
  Branch       :: (Value -> Bool) -> WorkflowStep -> WorkflowStep -> WorkflowStep
  Parallel     :: [WorkflowStep] -> WorkflowStep

-- | Workflow errors
data WorkflowError
  = ToolNotFound Text [Text]
  | CapabilityMissing Text Text
  | TypeError Text Text
  | TimeoutError StepId
  | ValidationError Text
  | CircularDependency [StepId]
  | LLMError LLMProvider Text
  deriving (Show, Eq)
```

## Agent Creation API

```haskell
module AgenticFramework.Workflow.Builder where

-- | Builder monad for constructing agents
newtype AgentBuilder a = AgentBuilder
  (StateT BuilderState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Create agent from configuration
createAgent :: AgentConfig -> IO Agent
createAgent config = ...

-- | Build agent using DSL
buildAgent :: AgentBuilder () -> IO Agent
buildAgent builder = ...

-- | Add capability to agent being built
withCapability :: Text -> AgentBuilder ()
withCapability capabilityDesc = ...

-- | Add natural language capability with name
withNamedCapability :: Text -> Text -> AgentBuilder ()
withNamedCapability name description = ...

-- | Set workflow for agent
withWorkflow :: Workflow Response -> AgentBuilder ()
withWorkflow workflow = ...

-- | Add tool to agent
withTool :: Tool -> AgentBuilder ()
withTool tool = ...

-- | Set system prompt
withSystemPrompt :: Text -> AgentBuilder ()
withSystemPrompt prompt = ...

-- | Set LLM configuration
withLLM :: LLMConfig -> AgentBuilder ()
withLLM config = ...

-- | Load capabilities from file
loadCapabilities :: FilePath -> AgentBuilder ()
loadCapabilities path = ...
```

## Workflow Construction API

```haskell
module AgenticFramework.Workflow.DSL where

-- | Make LLM call with additional prompt
llmCall :: Text -> Workflow Response
llmCall additionalPrompt = ...

-- | Invoke tool with input
useTool :: ToolName -> Value -> Workflow Value
useTool toolName input = ...

-- | Apply capability to workflow section
withCapability :: Text -> Workflow a -> Workflow a
withCapability capName action = ...

-- | Get user prompt from context
getUserPrompt :: Workflow Text
getUserPrompt = asks ctxUserPrompt

-- | Get available tools
getTools :: Workflow [Tool]
getTools = asks ctxTools

-- | Branch based on condition
branch :: (Value -> Bool) -> Workflow a -> Workflow a -> Workflow a
branch condition thenBranch elseBranch = ...

-- | Execute steps in parallel
parallel :: [Workflow a] -> Workflow [a]
parallel workflows = ...

-- | Execute sub-workflow
subWorkflow :: Workflow a -> Workflow a
subWorkflow = id  -- Sub-workflows are just workflows

-- | Fail with error
failWith :: WorkflowError -> Workflow a
failWith err = ...

-- | Try with fallback
tryWorkflow :: Workflow a -> Workflow a -> Workflow a
tryWorkflow primary fallback = ...
```

## Execution API

```haskell
module AgenticFramework.Workflow.Execution where

-- | Execute agent with user input
executeAgent :: Agent caps -> Text -> IO Response
executeAgent agent userPrompt = ...

-- | Run workflow with context
runWorkflow :: Workflow a -> AgentContext -> IO (Either WorkflowError a)
runWorkflow workflow context = ...

-- | Execute with timeout
executeWithTimeout :: Int -> Agent caps -> Text -> IO (Either WorkflowError Response)
executeWithTimeout timeoutMs agent prompt = ...

-- | Execute agent in traditional ReAct mode
executeTraditional :: Agent caps -> Text -> IO Response
executeTraditional agent prompt = ...

-- | Execute agent with explicit mode selection
executeWithMode :: ExecutionMode -> Agent caps -> Text -> IO Response
executeWithMode mode agent prompt = ...

data ExecutionMode = WorkflowMode | ReactMode | AutoDetect
```

## Capability Management API

```haskell
module AgenticFramework.Workflow.Capabilities where

-- | Load capability from JSON file
loadCapability :: FilePath -> IO (Either Text Capability)
loadCapability path = ...

-- | Load all capabilities from directory
loadCapabilitiesDir :: FilePath -> IO [Capability]
loadCapabilitiesDir dir = ...

-- | Create capability from natural language
createCapability :: Text -> Text -> Capability
createCapability name description = ...

-- | Combine multiple capabilities
combineCapabilities :: [Capability] -> Capability
combineCapabilities caps = ...

-- | Apply capability to prompt
applyCapability :: Capability -> Prompt -> Prompt
applyCapability cap prompt = ...

-- | Check if capability is available
hasCapability :: Text -> AgentContext -> Bool
hasCapability capName ctx = ...
```

## Tool Integration API

```haskell
module AgenticFramework.Workflow.Tools where

-- | Tool executor typeclass
class ToolExecutor m where
  invokeTool :: Tool -> ToolInput -> m (Either ToolError ToolOutput)

-- | Direct IO execution
instance ToolExecutor IO where ...

-- | LangChain execution
instance (LLM llm) => ToolExecutor (llm -> IO) where ...

-- | Create tool from function
createTool :: Text -> Text -> (Value -> IO Value) -> Tool
createTool name desc fn = ...

-- | Create LangChain-compatible tool
createLangChainTool :: Text -> Text -> ToolSchema -> Tool
createLangChainTool name desc schema = ...

-- | Find tool by name
findTool :: ToolName -> [Tool] -> Maybe Tool
findTool name tools = ...

-- | Validate tool input
validateToolInput :: Tool -> Value -> Either ValidationError Value
validateToolInput tool input = ...
```

## Validation API

```haskell
module AgenticFramework.Workflow.Validation where

-- | Validate workflow before execution
validateWorkflow :: Workflow a -> Either [ValidationError] ()
validateWorkflow workflow = ...

-- | Check type compatibility
checkTypeCompatibility :: WorkflowStep -> WorkflowStep -> Bool
checkTypeCompatibility from to = ...

-- | Detect circular dependencies
detectCircularDeps :: [WorkflowStep] -> Maybe [StepId]
detectCircularDeps steps = ...

-- | Validate agent configuration
validateAgent :: Agent caps -> Either [ValidationError] ()
validateAgent agent = ...

-- | Validate capability definition
validateCapability :: Capability -> Either ValidationError ()
validateCapability cap = ...
```

## Error Handling API

```haskell
module AgenticFramework.Workflow.Errors where

-- | Format error for user display
formatError :: WorkflowError -> Text
formatError err = ...

-- | Get error context
getErrorContext :: WorkflowError -> ErrorContext
getErrorContext err = ...

-- | Create detailed error report
createErrorReport :: WorkflowError -> WorkflowState -> Text
createErrorReport err state = ...

-- | Error recovery strategies
recoverFrom :: WorkflowError -> Workflow a -> Workflow a
recoverFrom err recovery = ...

-- | Accumulate errors without failing
collectErrors :: [Workflow a] -> Workflow ([WorkflowError], [a])
collectErrors workflows = ...
```

## Usage Examples

### Basic Agent Creation

```haskell
-- Create simple agent with capabilities
simpleAgent <- buildAgent $ do
  withSystemPrompt "You are a helpful assistant"
  withCapability "Think step-by-step before answering"
  withCapability "Be concise but thorough"
  withLLM defaultLLMConfig

-- Execute agent
response <- executeAgent simpleAgent "What is quantum computing?"
```

### Agent with Workflow

```haskell
-- Define workflow
researchWorkflow :: Workflow Response
researchWorkflow = do
  query <- getUserPrompt

  -- Initial analysis with reasoning
  analysis <- withCapability "reasoning" $
    llmCall $ "Analyze query: " <> query

  -- Search for information
  results <- useTool "web_search" analysis

  -- Validate and synthesize
  validated <- withCapability "validation" $
    llmCall $ "Validate and synthesize: " <> results

  return validated

-- Create agent with workflow
researchAgent <- buildAgent $ do
  withSystemPrompt "You are a research assistant"
  withWorkflow researchWorkflow
  withTool webSearchTool
  withCapability "reasoning"
  withCapability "validation"

-- Execute
result <- executeAgent researchAgent "Latest advances in AI"
```

### Dynamic Capability Loading

```haskell
-- Load capabilities from configuration
agent <- buildAgent $ do
  withSystemPrompt "Multi-capability agent"
  loadCapabilities "~/.config/agent/capabilities/"
  withLLM claudeConfig

-- Capabilities are loaded at runtime from JSON files
```

### Error Handling

```haskell
-- Execute with error handling
result <- runWorkflow myWorkflow context
case result of
  Left err -> putStrLn $ formatError err
  Right value -> print value

-- With recovery
resultWithRecovery <- runWorkflow
  (tryWorkflow primaryWorkflow fallbackWorkflow)
  context
```

## Version Compatibility

- **Minimum GHC**: 9.10.1
- **Dependencies**: mtl >= 2.3, transformers >= 0.6, aeson >= 2.2
- **Breaking changes**: Will follow PVP (Package Versioning Policy)

## Migration Guide

For users of existing AgenticFramework:

1. Existing `Agent` type is extended, not replaced
2. `executeAgent` maintains backward compatibility
3. New workflow features are opt-in via `agentWorkflow` field
4. Traditional execution available via `executeTraditional`

No breaking changes for existing code.