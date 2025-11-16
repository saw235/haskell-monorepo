# Implementation Plan: Agentic Framework Library

**Branch**: `002-agentic-framework` | **Date**: 2025-11-15 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/002-agentic-framework/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

A Haskell library for building autonomous agents with tool access, skill loading, multi-agent orchestration, and comprehensive observability. Built as a higher-level abstraction over langchain-hs, providing type-safe agent definitions, automatic context window management, structured logging, and composable orchestration patterns (sequential, parallel, agent-as-tool). Key technical approach: FFI integration with Rust tokenizers for accurate token counting, monad-logger for observability, lazy-loaded skill system to minimize context usage, and automatic context summarization at 90% threshold.

## Technical Context

**Language/Version**: Haskell GHC 9.10.1 (Stackage LTS-24.19)
**Primary Dependencies**: langchain-hs (LLM integration, ReAct agents), monad-logger (logging), ansi-terminal (colorized output), aeson (JSON), http-conduit (web search), Rust FFI (tiktoken-rs or HuggingFace tokenizers)
**Storage**: File-based (skill markdown files, logs to file system), no database required
**Testing**: Hspec for unit tests, QuickCheck for property-based testing, integration tests with mock LLM responses
**Target Platform**: Linux/macOS/Windows server environments (library for Haskell applications)
**Project Type**: Haskell library (single project in haskell/libs/)
**Performance Goals**:
- Single agent task completion: <30s for 3-5 tool calls
- Skill loading overhead: <500ms
- Token counting overhead: <100ms per invocation
- Context summarization: <5s
- Parallel agent scaling: linear up to 5 agents

**Constraints**:
- Token counting accuracy within 5% of actual model counts
- Context summarization must reduce usage by ≥50%
- Maximum nested agent depth: 5 levels
- Default tool timeout: 10 seconds
- Context summarization triggers at 90% of token limit
- Must integrate with langchain-hs without forking

**Scale/Scope**:
- Support 3-5 major LLM providers (Ollama, OpenAI, Claude, etc.)
- 10+ built-in tools (file ops, web search, calculator, etc.)
- Unlimited custom tools via interface
- Unlimited skill documents (lazy-loaded)
- 5 concurrent parallel agents recommended maximum
- 5 nested agent call depth maximum

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Principle Compliance

| Principle | Status | Notes |
|-----------|--------|-------|
| **I. Bazel-First Development** | ✅ PASS | Library will reside in `haskell/libs/agentic-framework/` with BUILD.bazel defining haskell_library rule. All Haskell dependencies via Stackage pinning, Rust FFI managed through Bazel external dependencies. |
| **II. Polyglot Integration** | ⚠️  REVIEW | Requires Rust FFI integration for tokenizers. This follows existing pattern (external dependencies in Bazel), but adds new FFI complexity. Justification: Production-quality tokenization requires battle-tested implementations; pure Haskell BPE would be prohibitively complex. |
| **III. Library-Centric Architecture** | ✅ PASS | Self-contained library with clear purpose (agentic framework), public visibility for app consumption, documented API surface. Lives in `haskell/libs/agentic-framework/`. |
| **IV. Test-Driven Quality** | ✅ PASS | QuickCheck for property-based testing (token counting accuracy, summarization preservation), Hspec for unit tests, integration tests with fixture data (no live LLM calls in unit tests). |
| **V. Automated Formatting** | ✅ PASS | Ormolu for all Haskell code, Buildifier for BUILD.bazel, automated in CI. |

### Architecture Standards Compliance

| Standard | Status | Notes |
|----------|--------|-------|
| **Applications in haskell/app/** | ✅ PASS | This is a library, not an application. Will reside in `haskell/libs/agentic-framework/`. |
| **External Dependencies via Bazel** | ✅ PASS | Rust tokenizer FFI will be managed through Bazel external dependencies mechanism (similar to Selenium/ChromeDriver pattern). |
| **Docker for web scraping** | N/A | Library may be used by web scraping apps, but library itself doesn't require Docker. |

### Gate Decision: **APPROVED** with justification

The Rust FFI requirement (Principle II) is the only complexity addition. This is justified because:
1. **Simpler Alternative Rejected**: Pure Haskell BPE tokenizer implementation would require ~2000 LOC, ongoing maintenance for model updates, and accuracy verification burden.
2. **Existing Pattern**: Project already uses FFI-style external dependencies (Selenium server JAR), so this follows established patterns.
3. **Production Necessity**: Accurate token counting is critical for context management; 5% accuracy requirement cannot be met with heuristic approaches.

## Project Structure

### Documentation (this feature)

```text
specs/002-agentic-framework/
├── spec.md              # Feature specification (input)
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (technical decisions)
├── data-model.md        # Phase 1 output (entities & types)
├── quickstart.md        # Phase 1 output (developer onboarding)
├── contracts/           # Phase 1 output (API contracts)
│   ├── agent-api.md     # Agent creation and execution API
│   ├── tool-api.md      # Tool system API
│   ├── skill-api.md     # Skill management API
│   ├── orchestration-api.md  # Multi-agent orchestration API
│   └── observability-api.md  # Logging and metrics API
└── tasks.md             # Phase 2 output (/speckit.tasks - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
haskell/libs/agentic-framework/
├── BUILD.bazel                    # Bazel library definition
├── README.md                      # Library overview and quick start
├── src/
│   ├── AgenticFramework.hs       # Main module (re-exports public API)
│   ├── AgenticFramework/
│   │   ├── Agent.hs              # Agent definition and execution
│   │   ├── Tool.hs               # Tool system (built-in + custom)
│   │   ├── Tool/
│   │   │   ├── File.hs           # File operations tools
│   │   │   ├── WebSearch.hs     # Web search tool
│   │   │   └── LangChain.hs     # Wrappers for langchain-hs tools
│   │   ├── Skill.hs              # Skill loading and management
│   │   ├── Orchestration.hs     # Multi-agent coordination
│   │   ├── Orchestration/
│   │   │   ├── Sequential.hs    # Sequential workflow
│   │   │   ├── Parallel.hs      # Parallel execution
│   │   │   └── Handoff.hs       # HandoffObject and context passing
│   │   ├── Context.hs            # AgentContext and token management
│   │   ├── Context/
│   │   │   ├── Tokenizer.hs     # FFI to Rust tokenizers
│   │   │   └── Summarization.hs # Context summarization logic
│   │   ├── Logging.hs            # Observability and structured logging
│   │   ├── Types.hs              # Shared types (TokenMetrics, etc.)
│   │   └── Internal/             # Internal helpers (not exported)
│   └── skills/                   # Example skill files for testing
│       └── example-debugging.md
├── test/
│   ├── AgenticFramework/
│   │   ├── AgentSpec.hs          # Agent execution tests
│   │   ├── ToolSpec.hs           # Tool system tests
│   │   ├── SkillSpec.hs          # Skill loading tests
│   │   ├── OrchestrationSpec.hs # Multi-agent workflow tests
│   │   ├── ContextSpec.hs        # Token management & summarization
│   │   └── LoggingSpec.hs        # Logging tests
│   ├── Integration/
│   │   ├── EndToEndSpec.hs      # Full agent workflows
│   │   └── Fixtures/            # Mock LLM responses, test skills
│   └── Property/
│       ├── TokenizerProps.hs    # Token counting accuracy properties
│       └── SummarizationProps.hs # Context preservation properties
└── examples/
    ├── SimpleAgent.hs            # Basic agent with tools
    ├── SkillDemo.hs              # Agent using skills
    └── MultiAgentWorkflow.hs    # Sequential orchestration demo
```

**Structure Decision**: Single library project following standard Haskell library layout. Main public API exported from `AgenticFramework.hs` with sub-modules for each major component. Internal implementation details in `Internal/` directory (not exported). FFI tokenizer integration managed through separate module with clear boundary. Examples directory provides executable demonstrations of library usage.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| Rust FFI for tokenizers | Accurate model-specific token counting required for context management (FR-039, FR-040); 5% accuracy target (SC-011) | Pure Haskell BPE implementation: ~2000 LOC, complex maintenance for model updates, accuracy verification burden. Character-based estimation: Cannot meet 5% accuracy requirement. |
| monad-logger + pluggable handlers | Ecosystem integration (FR-029a) + developer customization (FR-029b) required per spec | Single logging approach: Spec explicitly requires both monad-logger integration AND pluggable custom handlers (FR-029a, FR-029b). |
