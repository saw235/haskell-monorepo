# Quickstart: Agentic Framework Library

**Target Audience**: Haskell developers new to the agentic-framework library
**Time to Complete**: 10-15 minutes
**Prerequisites**: Haskell GHC 9.10.1, Bazel 7.4.0, basic Haskell knowledge

---

## Installation

### 1. Add to your BUILD.bazel

```python
haskell_binary(
    name = "my-agent-app",
    srcs = ["Main.hs"],
    deps = [
        "//haskell/libs/agentic-framework",
    ],
)
```

### 2. Verify Installation

```bash
bazel build //your/app:my-agent-app
```

---

## Your First Agent (5 minutes)

### Step 1: Create a Simple Agent

```haskell
-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

import AgenticFramework
import AgenticFramework.Tool (calculatorTool, wikipediaTool)

main :: IO ()
main = do
  -- Configure LLM (using Ollama local model)
  let llmConfig = LLMConfig
        { llmProvider = Ollama
        , llmModel = "llama2"
        , llmApiKey = Nothing        -- Ollama doesn't need API key
        , llmBaseUrl = Just "http://localhost:11434"
        , llmMaxTokens = 4096
        , llmTemperature = 0.7
        }

  -- Create agent configuration
  let agentConfig = AgentConfig
        { configName = "assistant"
        , configSystemPrompt = "You are a helpful assistant. Use tools to answer questions accurately."
        , configTools = [calculatorTool, wikipediaTool]
        , configLLM = llmConfig
        , configSkillsDir = Nothing
        , configMaxTokens = Nothing
        , configTemperature = Nothing
        }

  -- Create and run agent
  agent <- createAgent agentConfig
  result <- executeAgent agent "What is 15 * 23? Then tell me about Haskell."

  -- Print result
  putStrLn $ "Agent response: " <> resultOutput result
  putStrLn $ "Success: " <> show (resultSuccess result)
```

### Step 2: Run Your Agent

```bash
bazel run //your/app:my-agent-app
```

**Expected Output**:
```text
Agent response: 15 * 23 = 345. Haskell is a purely functional programming language...
Success: True
```

**What Just Happened?**
1. Agent received your question
2. Reasoned that it needs calculator tool for math
3. Called `calculatorTool` with "15 * 23"
4. Reasoned that it needs Wikipedia for Haskell info
5. Called `wikipediaTool` with "Haskell"
6. Synthesized final answer from both tool outputs

---

## Adding Custom Tools (3 minutes)

### Example: Weather Tool

```haskell
import qualified Data.HashMap.Strict as HM
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
      case HM.lookup "city" (unToolInput input) of
        Nothing -> return $ Left $ ToolValidationError "Missing city"
        Just (A.String city) -> do
          -- In real app, call weather API
          return $ Right $ ToolOutput $ HM.fromList
            [ ("temperature", A.Number 72)
            , ("conditions", A.String "sunny")
            ]
        _ -> return $ Left $ ToolValidationError "City must be string"
  , toolConfigTimeout = Just 5_000_000  -- 5 seconds
  , toolConfigRetryable = True
  }

-- Add to agent
let agentConfig = defaultAgentConfig "assistant" "You are helpful" llmConfig
                & addTool weatherTool
                & addTool calculatorTool
```

---

## Using Skills (4 minutes)

### Step 1: Create a Skill File

Create `skills/debugging.md`:

```markdown
---
label: debugging-checklist
description: Systematic approach to debugging runtime errors
category: debugging
---

## Purpose
Help developers methodically isolate and fix bugs.

## Methodology
1. Reproduce the issue reliably
2. Check error messages and logs
3. Isolate the failing component
4. Add targeted logging/breakpoints
5. Form hypothesis and test
6. Verify fix doesn't introduce regressions

## Examples
### Example: NullPointerException
- Look for uninitialized variables
- Check for missing null checks
- Verify object lifecycle
```

### Step 2: Load Skills and Use Them

```haskell
import AgenticFramework.Skill

main :: IO ()
main = do
  -- Load skills from directory
  skills <- loadSkillsFromDirectory "./skills"
  putStrLn $ "Loaded " <> show (length skills) <> " skills"

  -- Create agent with skills directory
  let agentConfig = AgentConfig
        { configName = "debugger"
        , configSystemPrompt = "You are an expert debugger. Use available skills to help users."
        , configTools = [readFileTool, writeFileTool]
        , configLLM = llmConfig
        , configSkillsDir = Just "./skills"  -- Skills auto-loaded on agent creation
        , configMaxTokens = Nothing
        , configTemperature = Nothing
        }

  agent <- createAgent agentConfig
  result <- executeAgent agent "I'm getting a segfault. Help me debug it."

  -- Agent will reference debugging-checklist skill in its response
  putStrLn $ resultOutput result
```

**Skills are lazy-loaded**: Only labels and descriptions loaded initially; full content loaded when agent requests the skill.

---

## Multi-Agent Orchestration (5 minutes)

### Sequential Workflow

```haskell
import AgenticFramework.Orchestration

main :: IO ()
main = do
  -- Create specialized agents
  let analyzerAgent = createAgent AgentConfig
        { configName = "analyzer"
        , configSystemPrompt = "You analyze requirements and extract key points."
        , configTools = []
        , configLLM = llmConfig
        , ...
        }

  let coderAgent = createAgent AgentConfig
        { configName = "coder"
        , configSystemPrompt = "You write clean, well-documented code based on requirements."
        , configTools = [writeFileTool]
        , configLLM = llmConfig
        , ...
        }

  let reviewerAgent = createAgent AgentConfig
        { configName = "reviewer"
        , configSystemPrompt = "You review code for bugs, style, and best practices."
        , configTools = [readFileTool]
        , configLLM = llmConfig
        , ...
        }

  -- Execute sequential workflow
  results <- executeSequential
    [analyzerAgent, coderAgent, reviewerAgent]
    "Build a function to calculate Fibonacci numbers"

  -- Each agent builds on previous agent's work via HandoffObject
  forM_ (zip [1..] results) $ \(i, result) -> do
    putStrLn $ "Agent " <> show i <> " output:"
    putStrLn $ resultOutput result
    putStrLn ""
```

### Parallel Execution

```haskell
-- Get multiple perspectives
let pythonCoder = createAgent pythonConfig
    haskellCoder = createAgent haskellConfig
    rustCoder = createAgent rustConfig

results <- executeParallel
  [pythonCoder, haskellCoder, rustCoder]
  "Implement binary search"

-- Now you have 3 different implementations to compare
```

---

## Logging and Observability

### Enable Logging

```haskell
import AgenticFramework.Logging

main :: IO ()
main = do
  let logConfig = LogConfig
        { logLevel = INFO
        , logHandlers = [colorizedStdoutHandler, fileHandler "./agent.log"]
        , logColorize = True
        }

  runWithLogging logConfig $ do
    agent <- liftIO $ createAgent agentConfig
    result <- liftIO $ executeAgent agent "Calculate 2 + 2"

    -- Logs automatically captured
    liftIO $ putStrLn $ resultOutput result
```

**Console Output** (colorized):
```text
[10:30:00] INFO  [agent-123] Starting agent execution
[10:30:01] DEBUG [agent-123] Selected tool: calculator
[10:30:01] INFO  [agent-123] Tool calculator: Execution completed in 5ms
[10:30:02] INFO  [agent-123] Agent execution completed successfully
```

### Check Token Usage

```haskell
result <- executeAgent agent "Long conversation..."
let metrics = getTokenMetrics (resultContext result)

putStrLn $ "Tokens used: " <> show (currentTokenCount metrics)
        <> " / " <> show (modelTokenLimit metrics)
putStrLn $ "Percentage: " <> show (percentageUsed metrics) <> "%"

when (summarizationTriggered metrics) $
  putStrLn "Note: Context was automatically summarized to save space"
```

---

## Common Patterns

### Pattern 1: Error Handling

```haskell
import Control.Exception (try, SomeException)

safeExecute :: Agent -> Text -> IO (Either String AgentResult)
safeExecute agent input = do
  result <- try (executeAgent agent input)
  case result of
    Left (e :: SomeException) -> return $ Left (show e)
    Right r -> return $ Right r
```

### Pattern 2: Conversational Agent

```haskell
conversationLoop :: Agent -> AgentContext -> IO ()
conversationLoop agent ctx = do
  putStr "You: "
  input <- getLine
  when (input /= "quit") $ do
    result <- executeAgentWithContext agent ctx input
    putStrLn $ "Agent: " <> resultOutput result
    conversationLoop agent (resultContext result)  -- Pass updated context

main :: IO ()
main = do
  agent <- createAgent agentConfig
  initialResult <- executeAgent agent "Hello!"
  putStrLn $ "Agent: " <> resultOutput initialResult
  conversationLoop agent (resultContext initialResult)
```

### Pattern 3: Agent with Retry Logic

```haskell
executeWithRetry :: Agent -> Text -> Int -> IO AgentResult
executeWithRetry agent input maxRetries = go 0
  where
    go attempt = do
      result <- executeAgent agent input
      if resultSuccess result || attempt >= maxRetries
        then return result
        else do
          putStrLn $ "Attempt " <> show (attempt + 1) <> " failed, retrying..."
          go (attempt + 1)
```

---

## Next Steps

1. **Read API Contracts**: See `contracts/` directory for detailed API documentation
2. **Explore Examples**: Check `haskell/libs/agentic-framework/examples/` for more patterns
3. **Review Data Model**: See `data-model.md` for type details
4. **Customize Logging**: Create custom LogHandler for your infrastructure
5. **Build Complex Workflows**: Combine sequential + parallel orchestration

---

## Troubleshooting

### "LLM Connection Error"
- **Ollama**: Ensure Ollama is running: `ollama serve`
- **OpenAI**: Check API key in environment: `export OPENAI_API_KEY=sk-...`
- **Network**: Verify `llmBaseUrl` is reachable

### "Tool Timeout"
- Increase timeout: `toolConfigTimeout = Just 30_000_000` (30s)
- Check tool implementation for blocking operations

### "Context Window Exceeded"
- Automatic summarization triggers at 90%, but you can lower:
  ```haskell
  let agent = createAgent config { contextThreshold = 0.8 }  -- 80% threshold
  ```

### "Circular Dependency Error"
- Review agent-as-tool calls: ensure no cycles
- Reduce nesting depth if hitting 5-level limit

---

## Resources

- **Spec**: `spec.md` - Complete feature specification
- **Research**: `research.md` - Technical decisions and alternatives
- **Contracts**: `contracts/*.md` - Detailed API documentation
- **Data Model**: `data-model.md` - Type definitions and relationships

**Need Help?** Open an issue or check langchain-hs documentation for LLM configuration.
