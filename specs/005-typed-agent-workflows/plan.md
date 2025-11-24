# Implementation Plan: Typed Agent Workflows

**Branch**: `005-typed-agent-workflows` | **Date**: 2025-11-16 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/005-typed-agent-workflows/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Enable developers to create AI agents with typed workflows and natural language capabilities, building on the existing agentic-framework library to add monad-based workflow composition while maintaining backward compatibility with traditional ReAct-style execution.

## Technical Context

**Language/Version**: Haskell with GHC 9.10.1 (Stackage LTS-24.19)
**Primary Dependencies**: mtl, transformers, monad-logger, langchain-hs, aeson, text, containers
**Storage**: File-based capability definitions (JSON/YAML), in-memory workflow state
**Testing**: QuickCheck property-based testing, HSpec unit tests
**Target Platform**: Linux/macOS/Windows (cross-platform Haskell)
**Project Type**: Library extension to existing agentic-framework
**Performance Goals**: <100ms overhead per workflow step, support 20+ capabilities without degradation
**Constraints**: Must maintain backward compatibility with existing Agent type, type safety for workflows
**Scale/Scope**: Extend existing ~1000 LOC library with ~2000 LOC for workflow system

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### I. Bazel-First Development ✅
- Will use existing BUILD.bazel structure
- Dependencies already available (mtl, transformers in BUILD)
- No new build system changes required

### II. Polyglot Integration ✅
- Pure Haskell extension to existing library
- No cross-language concerns for this feature
- Maintains existing LangChain integration patterns

### III. Library-Centric Architecture ✅
- Extends existing agentic-framework library
- Workflow module will be self-contained
- Public API for agent creation with workflows

### IV. Test-Driven Quality ✅
- Will add QuickCheck properties for workflow composition
- Unit tests for capability application
- Integration tests with existing tools

### V. Automated Formatting ✅
- Ormolu formatting for all new Haskell code
- Consistent with existing codebase style

### VI. Requirements Traceability ✅
- Tasks will be tagged with FR-001 through FR-015
- Test tasks will reference validated requirements

**Constitution Status**: PASSES all gates

## Project Structure

### Documentation (this feature)

```text
specs/005-typed-agent-workflows/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
haskell/libs/agentic-framework/
├── src/
│   └── AgenticFramework/
│       ├── Workflow.hs           # New: Core workflow monad and execution
│       ├── Workflow/
│       │   ├── Types.hs          # New: Workflow types and data structures
│       │   ├── Execution.hs      # New: Workflow execution engine
│       │   ├── Capabilities.hs   # New: Capability application logic
│       │   └── Builder.hs        # New: DSL for workflow construction
│       ├── Agent.hs              # Modified: Add workflow support
│       ├── Types.hs              # Modified: Add workflow-related types
│       └── Capabilities.hs       # Existing: Move and extend capability system
├── test/
│   ├── Workflow/
│   │   ├── TypeSafetySpec.hs    # New: Type safety properties
│   │   ├── CompositionSpec.hs   # New: Workflow composition tests
│   │   └── CapabilitySpec.hs    # New: Capability application tests
│   └── IntegrationSpec.hs       # Modified: Add workflow integration tests
└── examples/
    ├── ResearchAgent.hs          # New: Research workflow example
    ├── DebugAgent.hs            # New: Debugging workflow example
    └── SimpleWorkflow.hs        # New: Basic workflow tutorial
```

**Structure Decision**: Extend the existing agentic-framework library with a new Workflow module and submodules, maintaining backward compatibility while adding the new typed workflow system alongside the existing imperative agent execution.

## Complexity Tracking

Not applicable - no Constitution violations requiring justification.

---

## Phase 0: Research & Unknowns

### Research Tasks Identified

1. **Capability Loading Mechanism** (FR-015)
   - Research: Best practices for runtime capability loading in Haskell
   - Options: JSON files, YAML config, environment variables
   - Decision needed: Primary loading mechanism

2. **Workflow Type Safety Patterns**
   - Research: Advanced type-level programming for workflow composition
   - Investigate: Indexed monads vs phantom types vs GADTs
   - Decision needed: Type safety approach for data flow

3. **LangChain Integration Details**
   - Research: Current langchain-hs API and integration patterns
   - Investigate: How to maintain tool compatibility in workflows
   - Decision needed: Tool invocation abstraction layer

4. **Error Handling Strategy**
   - Research: Best practices for error handling in monad stacks
   - Investigate: ExceptT vs custom error types
   - Decision needed: Error propagation through workflows

### Research Execution Plan

```text
For Capability Loading:
  Task: "Research runtime configuration loading patterns in Haskell production systems"
  Task: "Evaluate aeson vs yaml vs dhall for capability definitions"

For Type Safety:
  Task: "Find best practices for type-safe workflow DSLs in Haskell"
  Task: "Research successful implementations of indexed monads"

For LangChain:
  Task: "Analyze langchain-hs source for extension points"
  Task: "Research tool abstraction patterns for multiple LLM providers"

For Error Handling:
  Task: "Research monad transformer error handling patterns"
  Task: "Find examples of user-friendly error messages in Haskell libraries"
```

---

## Phase 1: Design & Contracts

### Core Type Design (from user input)

```haskell
-- Agent with traditional components PLUS workflow
data Agent caps = Agent
  { agentSystemPrompt :: Text          -- Base behavior/role
  , agentTools :: [Tool]               -- LangChain tools
  , agentCapabilities :: [Capability]  -- Natural language behavior modifiers
  , agentWorkflow :: Maybe (Workflow Response)  -- Optional structured workflow
  , agentLLM :: LLMConfig              -- Which LLM to use
  }

-- Workflow monad with access to agent context
newtype Workflow a = Workflow
  (ReaderT AgentContext (StateT WorkflowState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- Context available to workflows
data AgentContext = AgentContext
  { ctxSystemPrompt :: Text
  , ctxUserPrompt :: Text
  , ctxTools :: [Tool]
  , ctxCapabilities :: [Capability]
  , ctxLLM :: LLMConfig
  }

-- Capability as natural language modifier
data Capability = Capability
  { capName :: Text
  , capDescription :: Text
  , capModifier :: Prompt -> Prompt
  }
```

### API Contract Design

The primary API will be a Haskell library interface (no REST/GraphQL needed for this feature):

```haskell
-- Agent creation API
createAgent :: AgentConfig -> IO Agent
buildAgent :: AgentBuilder () -> IO Agent
withCapability :: Text -> AgentBuilder ()
withWorkflow :: Workflow Response -> AgentBuilder ()

-- Workflow construction API
llmCall :: Text -> Workflow Response
useTool :: ToolName -> Value -> Workflow Value
withCapability :: Text -> Workflow a -> Workflow a

-- Execution API
executeAgent :: Agent -> Text -> IO Response
runWorkflow :: Workflow a -> AgentContext -> IO a
```

### Integration Points

- **Existing Agent.hs**: Modify to support optional workflows
- **Existing Tool.hs**: Ensure tools work within workflow context
- **Existing Types.hs**: Add workflow-related types
- **New Workflow module**: Core workflow functionality

---

## Phase 2: Tasks Generation Preview

Tasks will be generated by `/speckit.tasks` command and will include:

1. Core workflow monad implementation (FR-002, FR-006)
2. Capability application system (FR-001, FR-004, FR-007)
3. Workflow builder DSL (FR-002, FR-010, FR-014)
4. Agent type extension (FR-003, FR-005)
5. Execution engine with both modes (FR-005, FR-008, FR-009)
6. Type safety validation (FR-006, FR-013)
7. Error handling and messaging (FR-011)
8. Nested workflow support (FR-012)
9. Dynamic capability loading (FR-015)
10. Example implementations
11. Comprehensive test suite

Each task will be tagged with the appropriate FR-XXX identifiers for traceability.