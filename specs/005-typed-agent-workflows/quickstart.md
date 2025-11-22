# Quick Start: Typed Agent Workflows

Get started with typed agent workflows in 5 minutes!

## Installation

Add to your `BUILD.bazel`:

```python
haskell_library(
    name = "my-agent-lib",
    deps = [
        "//haskell/libs/agentic-framework",
        "//:mtl",
        "//:transformers",
        "//:aeson",
    ],
    # ... other configuration
)
```

## Basic Agent (No Workflow)

Create a simple agent with natural language capabilities:

```haskell
import AgenticFramework.Workflow

main :: IO ()
main = do
  -- Build agent with capabilities
  agent <- buildAgent $ do
    withSystemPrompt "You are a helpful coding assistant"
    withCapability "Break down problems step-by-step"
    withCapability "Provide code examples when relevant"
    withLLM defaultLLMConfig

  -- Execute agent
  response <- executeAgent agent "How do I parse JSON in Haskell?"
  putStrLn response
```

## Agent with Typed Workflow

Create an agent with a structured workflow:

```haskell
import AgenticFramework.Workflow
import AgenticFramework.Workflow.DSL

-- Define a workflow with type-safe data flow
analysisWorkflow :: Workflow Response
analysisWorkflow = do
  -- Get user input
  userQuery <- getUserPrompt

  -- Step 1: Understand the query with reasoning
  understanding <- withCapability "reasoning" $
    llmCall $ "What is the user asking for: " <> userQuery

  -- Step 2: Gather information using tools
  searchResults <- useTool "web_search" understanding

  -- Step 3: Validate and synthesize
  synthesis <- withCapability "validation" $
    llmCall $ "Synthesize findings: " <> searchResults

  -- Step 4: Generate response
  llmCall $ "Create helpful response: " <> synthesis

main :: IO ()
main = do
  -- Create agent with workflow
  agent <- buildAgent $ do
    withSystemPrompt "Research assistant"
    withWorkflow analysisWorkflow
    withTool webSearchTool
    withCapability "reasoning"
    withCapability "validation"

  -- Execute (automatically uses workflow)
  result <- executeAgent agent "What are monads in Haskell?"
  putStrLn result
```

## Conditional Workflows

Workflows with branching logic:

```haskell
-- Workflow that adapts based on input
adaptiveWorkflow :: Workflow Response
adaptiveWorkflow = do
  query <- getUserPrompt

  -- Classify the query
  classification <- llmCall $ "Classify as technical/general: " <> query

  -- Branch based on classification
  branch (\v -> v == "technical")
    (technicalWorkflow query)  -- Technical path
    (generalWorkflow query)     -- General path

technicalWorkflow :: Text -> Workflow Response
technicalWorkflow query = do
  -- Use code search and documentation
  docs <- useTool "search_docs" query
  withCapability "technical-writing" $
    llmCall $ "Technical explanation of: " <> docs

generalWorkflow :: Text -> Workflow Response
generalWorkflow query = do
  -- Simple response with examples
  withCapability "simple-explanation" $
    llmCall $ "Explain simply: " <> query
```

## Loading Capabilities from Files

Create a capability file (`~/.config/agentic-framework/capabilities/reasoning.json`):

```json
{
  "name": "reasoning",
  "description": "Think through problems step-by-step, considering multiple angles",
  "parameters": {
    "style": "chain-of-thought",
    "depth": 3
  }
}
```

Load it in your agent:

```haskell
main :: IO ()
main = do
  agent <- buildAgent $ do
    withSystemPrompt "Advanced reasoner"
    loadCapabilities "~/.config/agentic-framework/capabilities/"
    withLLM claudeConfig

  -- Capabilities loaded from JSON files
  result <- executeAgent agent "Solve this logic puzzle..."
```

## Parallel Execution

Execute multiple operations concurrently:

```haskell
parallelResearch :: Workflow Response
parallelResearch = do
  query <- getUserPrompt

  -- Run searches in parallel
  results <- parallel
    [ useTool "web_search" query
    , useTool "arxiv_search" query
    , useTool "wikipedia" query
    ]

  -- Combine results
  let [web, arxiv, wiki] = results
  llmCall $ "Synthesize findings from: "
    <> "Web: " <> web
    <> "ArXiv: " <> arxiv
    <> "Wikipedia: " <> wiki
```

## Error Handling

Handle errors gracefully:

```haskell
safeWorkflow :: Workflow Response
safeWorkflow = do
  query <- getUserPrompt

  -- Try primary approach with fallback
  tryWorkflow
    (do  -- Primary: use external tools
      results <- useTool "advanced_search" query
      llmCall $ "Analyze: " <> results)
    (do  -- Fallback: use general knowledge
      withCapability "general-knowledge" $
        llmCall $ "Answer from general knowledge: " <> query)
```

## Capability Composition

Apply multiple capabilities to a single operation:

```haskell
composedWorkflow :: Workflow Response
composedWorkflow = do
  query <- getUserPrompt

  -- Apply multiple capabilities
  response <- withCapability "reasoning" $
               withCapability "creativity" $
               withCapability "conciseness" $
                 llmCall query

  return response
```

## Traditional vs Workflow Execution

Use both modes in the same application:

```haskell
main :: IO ()
main = do
  -- Agent supports both modes
  agent <- buildAgent $ do
    withSystemPrompt "Flexible assistant"
    withWorkflow complexWorkflow
    withTool searchTool

  -- Explicitly choose execution mode
  workflowResult <- executeWithMode WorkflowMode agent "Complex query"
  reactResult <- executeWithMode ReactMode agent "Simple query"

  -- Auto-detect based on query complexity
  autoResult <- executeAgent agent "Some query"
```

## Testing Workflows

Test your workflows independently:

```haskell
import Test.HSpec

spec :: Spec
spec = describe "Research Workflow" $ do
  it "handles search failures gracefully" $ do
    -- Create test context
    context <- createTestContext
      [ ("web_search", failingTool) ]

    -- Run workflow
    result <- runWorkflow researchWorkflow context

    -- Should fallback gracefully
    result `shouldSatisfy` isRight
```

## Next Steps

1. **Explore examples**: Check `/haskell/libs/agentic-framework/examples/`
2. **Read the API docs**: See [contracts/haskell-api.md](contracts/haskell-api.md)
3. **Learn about capabilities**: Understand how to create custom capabilities
4. **Build complex workflows**: Combine all features for sophisticated agents

## Common Patterns

### Research Agent Pattern
```haskell
search → validate → synthesize → respond
```

### Debug Agent Pattern
```haskell
observe → identify issue → search solutions → test → report
```

### Creative Agent Pattern
```haskell
brainstorm → evaluate → refine → present
```

## Troubleshooting

**Q: Workflow not executing?**
A: Check that `agentWorkflow` is set and not `Nothing`

**Q: Capability not applying?**
A: Verify capability name matches exactly (case-sensitive)

**Q: Tool not found?**
A: Ensure tool is added with `withTool` during agent building

**Q: Type errors in workflow?**
A: Use explicit type signatures for workflow steps

## Performance Tips

1. **Cache capabilities**: Loaded once at startup
2. **Reuse agents**: Don't rebuild for each request
3. **Parallel when possible**: Use `parallel` for independent operations
4. **Timeout long operations**: Set timeouts on tool calls

Happy workflow building! 🚀