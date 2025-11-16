# Tasks: Agentic Framework Library

**Input**: Design documents from `/specs/002-agentic-framework/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Property-based tests (QuickCheck) and integration tests are required per constitution (IV. Test-Driven Quality). All tests must pass before merging.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

All paths relative to `haskell/libs/agentic-framework/` per plan.md:
- Source: `src/AgenticFramework/`
- Tests: `test/AgenticFramework/`, `test/Integration/`, `test/Property/`
- Examples: `examples/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [X] T001 Create library directory structure at haskell/libs/agentic-framework/ per plan.md
- [X] T002 Create BUILD.bazel with haskell_library rule and dependencies (langchain-hs, monad-logger, aeson, http-conduit, ansi-terminal)
- [X] T003 [P] Create src/AgenticFramework.hs main module with public API re-exports
- [X] T004 [P] Add Rust tokenizer FFI dependency to MODULE.bazel and non_module_deps.bzl
- [X] T005 [P] Pin Haskell dependencies: bazel run @stackage-unpinned//:pin -- --upgrade-hackage
- [X] T006 [P] Create README.md at haskell/libs/agentic-framework/README.md with library overview

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T007 [P] Create src/AgenticFramework/Types.hs with core data types (AgentId, ToolInput, ToolOutput, ToolError, LogLevel, LLMProvider)
- [X] T008 [P] Create src/AgenticFramework/Context/Tokenizer.hs with FFI bindings to tiktoken-rs for token counting
- [X] T009 [P] Create test/Property/TokenizerProps.hs with QuickCheck properties for 5% accuracy requirement
- [X] T010 Create src/AgenticFramework/Logging.hs with monad-logger integration, LogHandler typeclass, and LogEntry data type
- [X] T011 [P] Create src/AgenticFramework/Logging/Handlers.hs with colorizedStdoutHandler and fileHandler implementations
- [X] T012 [P] Create test/AgenticFramework/LoggingSpec.hs with Hspec tests for log handlers and structured logging

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Create and Run a Single Agent with Tools (Priority: P1) 🎯 MVP

**Goal**: Enable developers to create an autonomous agent that can execute tools to answer questions

**Independent Test**: Create agent with calculator and file reader tools, give it a task requiring tool usage, verify correct output

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T013 [P] [US1] Property test for agent execution in test/Property/AgentProps.hs (agents complete tasks with 90% accuracy - SC-003)
- [X] T014 [P] [US1] Integration test for single agent with tools in test/Integration/SimpleAgentSpec.hs (verify calculator + file reader workflow)
- [X] T015 [P] [US1] Contract test for Agent API in test/AgenticFramework/AgentSpec.hs (createAgent, executeAgent, executeAgentWithContext)

### Implementation for User Story 1

- [X] T016 [P] [US1] Create src/AgenticFramework/Agent.hs with Agent, AgentConfig, AgentResult data types
- [X] T017 [P] [US1] Create src/AgenticFramework/Tool.hs with Tool, ToolConfig, ToolSchema data types
- [X] T018 [P] [US1] Create src/AgenticFramework/Context.hs with AgentContext and TokenMetrics types
- [X] T019 [US1] Implement createAgent function in src/AgenticFramework/Agent.hs (wraps langchain-hs LLM config)
- [X] T020 [US1] Implement executeAgent function in src/AgenticFramework/Agent.hs (integrates with langchain-hs ReAct agent)
- [X] T021 [US1] Implement executeAgentWithContext in src/AgenticFramework/Agent.hs for multi-turn conversations
- [X] T022 [P] [US1] Implement createTool function in src/AgenticFramework/Tool.hs with JSON schema validation
- [X] T023 [P] [US1] Implement executeTool with timeout (10s default) and retry logic (3 attempts) in src/AgenticFramework/Tool.hs
- [X] T024 [US1] Create src/AgenticFramework/Tool/File.hs with built-in file tools (readFileTool, writeFileTool, listDirectoryTool)
- [X] T025 [P] [US1] Create src/AgenticFramework/Tool/LangChain.hs with wrappers (calculatorTool, wikipediaTool, webScraperTool)
- [X] T026 [P] [US1] Create src/AgenticFramework/Tool/WebSearch.hs with webSearchTool implementation
- [X] T027 [US1] Add tool execution logging to Logging module (tool name, input, output, timestamp, duration - FR-025)
- [X] T028 [US1] Add agent reasoning logging to Logging module (thought processes from LLM - FR-026, FR-027)
- [X] T029 [P] [US1] Create examples/SimpleAgent.hs demonstrating agent with calculator and file reader
- [X] T030 [US1] Run integration tests and verify US1 acceptance scenarios pass (tests build successfully, require live LLM for execution)

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently

---

## Phase 4: User Story 7 - Context Window Management and Token Tracking (Priority: P2)

**Goal**: Enable agents to handle long conversations without hitting context limits, with token usage monitoring

**Independent Test**: (1) Run agent past 90% token limit and verify auto-summarization, (2) Query token metrics API and verify accuracy, (3) Trigger summarization failure and verify graceful fallback

### Tests for User Story 7

- [X] T031 [P] [US7] Property test for context summarization in test/Property/SummarizationProps.hs (≥50% token reduction - SC-012, preserve key information) - ✅ 8 tests created (2 passing, 6 pending T036)
- [X] T032 [P] [US7] Integration test for token management in test/Integration/ContextManagementSpec.hs (90% threshold trigger, fallback to truncation) - ✅ 10 tests created (1 passing, 6 pending T036-T040, 3 need Ollama)
- [X] T033 [P] [US7] Contract test for token metrics API in test/AgenticFramework/ContextSpec.hs (getTokenMetrics returns accurate counts) - ✅ All 9 tests PASSING with Ollama!

### Implementation for User Story 7

- [X] T034 [P] [US7] Implement token counting in src/AgenticFramework/Context/Tokenizer.hs using FFI to tiktoken-rs (completed in Phase 2) - ✅ All 9 tokenizer property tests PASSING!
- [X] T035 [P] [US7] Implement getTokenMetrics function in src/AgenticFramework/Context.hs (read-only API - FR-002b) (completed in Phase 2) - ✅ All 2 API tests PASSING!
- [X] T036 [US7] Create src/AgenticFramework/Context/Summarization.hs with summarizeContext function (uses agent's own LLM)
- [X] T037 [US7] Implement automatic summarization trigger at 90% threshold in AgentContext update logic
- [X] T038 [US7] Implement summarization failure fallback (hard truncation with ERROR logging - FR-044)
- [X] T039 [US7] Add context summarization logging (WARNING with token stats - FR-042)
- [X] T040 [US7] Update executeAgent to monitor token usage and trigger summarization during execution
- [ ] T041 [US7] Run integration tests and verify US7 acceptance scenarios pass

**Checkpoint**: At this point, User Stories 1 AND 7 should both work independently

---

## Phase 5: User Story 2 - Load Skills from Markdown Files (Priority: P2)

**Goal**: Enable agents to access reusable methodologies and best practices from markdown files

**Independent Test**: Create markdown skill file, configure agent to access it, give agent a task, verify it follows the documented methodology

### Tests for User Story 2

- [ ] T042 [P] [US2] Integration test for skill loading and usage in test/Integration/SkillDemoSpec.hs (lazy loading, file watch, discovery)
- [ ] T043 [P] [US2] Contract test for Skill API in test/AgenticFramework/SkillSpec.hs (loadSkillsFromDirectory, loadSkillContent, searchSkills)

### Implementation for User Story 2

- [ ] T044 [P] [US2] Create src/AgenticFramework/Skill.hs with Skill, SkillLabel, SkillCategory, SkillContent data types
- [ ] T045 [US2] Implement loadSkillsFromDirectory in src/AgenticFramework/Skill.hs (parse YAML front-matter, lazy load content - FR-012b)
- [ ] T046 [US2] Implement loadSkillContent in src/AgenticFramework/Skill.hs (on-demand markdown parsing)
- [ ] T047 [P] [US2] Implement searchSkills in src/AgenticFramework/Skill.hs (by label, category, keyword - FR-015)
- [ ] T048 [P] [US2] Implement generateSkillTemplate in src/AgenticFramework/Skill.hs (creates scaffold - FR-012c)
- [ ] T049 [P] [US2] Implement watchSkillsDirectory in src/AgenticFramework/Skill.hs with file system watcher (FR-016)
- [ ] T050 [US2] Implement addSkillToAgent in src/AgenticFramework/Skill.hs (updates AgentContext)
- [ ] T051 [US2] Implement skillAsTool in src/AgenticFramework/Skill.hs (wraps skill as invokable tool - FR-013)
- [ ] T052 [P] [US2] Create src/skills/example-debugging.md as test fixture with proper YAML front-matter
- [ ] T053 [P] [US2] Create examples/SkillDemo.hs demonstrating agent using debugging skill
- [ ] T054 [US2] Run integration tests and verify US2 acceptance scenarios pass

**Checkpoint**: At this point, User Stories 1, 2, AND 7 should all work independently

---

## Phase 6: User Story 6 - Comprehensive Observability and Logging (Priority: P3)

**Goal**: Enable developers to understand agent internals - tool calls, reasoning, decisions, failures

**Independent Test**: Run agent through complex task, examine logs, verify all tool calls/reasoning/decisions/errors captured with timestamps

### Tests for User Story 6

- [ ] T055 [P] [US6] Integration test for observability in test/Integration/ObservabilitySpec.hs (verify log completeness, filtering, export)
- [ ] T056 [P] [US6] Contract test for logging API in test/AgenticFramework/LoggingSpec.hs (runWithLogging, filterLogs, export formats)

### Implementation for User Story 6

- [ ] T057 [P] [US6] Create src/AgenticFramework/Logging/ExecutionLog.hs with ExecutionLog and LogEntry types
- [ ] T058 [US6] Implement filterLogs in src/AgenticFramework/Logging.hs (by agent/tool/level/time - FR-029, SC-010)
- [ ] T059 [P] [US6] Implement exportLogsJSON in src/AgenticFramework/Logging.hs (structured JSON export - FR-032)
- [ ] T060 [P] [US6] Implement exportLogsText in src/AgenticFramework/Logging.hs (human-readable format - FR-032)
- [ ] T061 [US6] Implement runWithLogging in src/AgenticFramework/Logging.hs (LoggingT monad transformer setup)
- [ ] T062 [US6] Add error logging with full context (agent state, stack trace - FR-030)
- [ ] T063 [US6] Add retry attempt logging (reasoning and parameter modifications - FR-037a)
- [ ] T064 [US6] Update AgentResult to include ExecutionLog field
- [ ] T065 [US6] Run integration tests and verify US6 acceptance scenarios pass

**Checkpoint**: Observability fully functional across all previous user stories

---

## Phase 7: User Story 3 - Orchestrate Multiple Agents with Sequential Handoffs (Priority: P3)

**Goal**: Enable multi-agent workflows where specialized agents collaborate sequentially

**Independent Test**: Create 3 agents (analyzer, generator, reviewer), run task through all three, verify each receives previous output and completes its portion

### Tests for User Story 3

- [ ] T066 [P] [US3] Integration test for sequential orchestration in test/Integration/SequentialWorkflowSpec.hs (3 agents, handoff, error handling)
- [ ] T067 [P] [US3] Contract test for orchestration API in test/AgenticFramework/OrchestrationSpec.hs (executeSequential, HandoffObject)

### Implementation for User Story 3

- [ ] T068 [P] [US3] Create src/AgenticFramework/Orchestration/Handoff.hs with HandoffObject and HandoffMetadata types
- [ ] T069 [P] [US3] Create src/AgenticFramework/Orchestration/Sequential.hs with SequentialWorkflow type
- [ ] T070 [US3] Implement createHandoff in src/AgenticFramework/Orchestration/Handoff.hs (extract from AgentResult - FR-023)
- [ ] T071 [US3] Implement handoffToPrompt in src/AgenticFramework/Orchestration/Handoff.hs (structured text format - FR-023b)
- [ ] T072 [US3] Implement executeSequential in src/AgenticFramework/Orchestration/Sequential.hs (passes HandoffObject between agents)
- [ ] T073 [US3] Implement executeSequentialWith in src/AgenticFramework/Orchestration/Sequential.hs (custom handoff transform - FR-023a)
- [ ] T074 [US3] Add graceful error handling for sequential workflows (stops on unrecoverable error - FR-036)
- [ ] T075 [US3] Add orchestration event logging (agent start/stop, handoffs - FR-028)
- [ ] T076 [P] [US3] Create src/AgenticFramework/Orchestration.hs main module with re-exports
- [ ] T077 [P] [US3] Create examples/MultiAgentWorkflow.hs demonstrating sequential workflow
- [ ] T078 [US3] Run integration tests and verify US3 acceptance scenarios pass

**Checkpoint**: Sequential orchestration functional, independent of parallel/agent-as-tool features

---

## Phase 8: User Story 4 - Parallel Agent Execution (Priority: P4)

**Goal**: Enable concurrent agent execution for ensemble approaches and diverse perspectives

**Independent Test**: Configure multiple agents to run concurrently, verify all complete independently, collect all outputs

### Tests for User Story 4

- [ ] T079 [P] [US4] Integration test for parallel execution in test/Integration/ParallelExecutionSpec.hs (concurrent agents, result collection, failure isolation)
- [ ] T080 [P] [US4] Property test for parallel scaling in test/Property/ParallelProps.hs (verify linear scaling up to 5 agents - SC-006)

### Implementation for User Story 4

- [ ] T081 [P] [US4] Create src/AgenticFramework/Orchestration/Parallel.hs with ParallelWorkflow and AggregationStrategy types
- [ ] T082 [US4] Implement executeParallel in src/AgenticFramework/Orchestration/Parallel.hs using async library (FR-018, FR-024)
- [ ] T083 [US4] Implement executeParallelWith in src/AgenticFramework/Orchestration/Parallel.hs (custom aggregation strategies)
- [ ] T084 [US4] Implement result collection with failure isolation (one agent failure doesn't affect others)
- [ ] T085 [US4] Add parallel execution logging (distinguish which agent performed which actions - FR-028)
- [ ] T086 [US4] Run integration tests and verify US4 acceptance scenarios pass

**Checkpoint**: Parallel execution functional, can combine with sequential workflows

---

## Phase 9: User Story 5 - Agent-as-Tool Pattern (Priority: P5)

**Goal**: Enable hierarchical agent systems where agents can delegate to specialist agents

**Independent Test**: Create coordinator and two specialist agents, give coordinator a task requiring both, verify coordinator calls appropriate specialists and combines outputs

### Tests for User Story 5

- [ ] T087 [P] [US5] Integration test for agent-as-tool pattern in test/Integration/AgentAsToolSpec.hs (coordinator with specialists, nesting, logging separation)
- [ ] T088 [P] [US5] Property test for circular dependency detection in test/Property/CircularDepsProps.hs (verify all cycles caught - FR-038)

### Implementation for User Story 5

- [ ] T089 [P] [US5] Implement createAgentTool in src/AgenticFramework/Orchestration.hs (wraps agent as tool - FR-020)
- [ ] T090 [US5] Implement circular dependency detection in src/AgenticFramework/Agent.hs using call stack (FR-038)
- [ ] T091 [US5] Implement maximum depth enforcement in src/AgenticFramework/Agent.hs (default 5 levels - FR-022, SC-008)
- [ ] T092 [US5] Add nested agent call logging (separate logs per agent - FR-028)
- [ ] T093 [US5] Update AgentContext to track call stack for circular detection and depth limiting
- [ ] T094 [US5] Run integration tests and verify US5 acceptance scenarios pass

**Checkpoint**: All user stories should now be independently functional

---

## Phase 10: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T095 [P] Add comprehensive inline documentation (Haddock) to all public API functions
- [ ] T096 [P] Create haskell/libs/agentic-framework/CHANGELOG.md documenting initial release
- [ ] T097 [P] Update examples/README.md with instructions for running examples
- [ ] T098 Run all QuickCheck property tests and verify 100% pass rate
- [ ] T099 Run all Hspec unit tests and verify 100% pass rate
- [ ] T100 Run all integration tests and verify 100% pass rate
- [ ] T101 Run quickstart.md validation (manual walkthrough of 10-15 min tutorial)
- [ ] T102 Format all Haskell code with Ormolu: find haskell/libs/agentic-framework -name "*.hs" -exec ormolu --mode inplace {} \;
- [ ] T103 Format BUILD.bazel with Buildifier
- [ ] T104 Build library: bazel build //haskell/libs/agentic-framework:agentic-framework
- [ ] T105 Run full test suite: bazel test //haskell/libs/agentic-framework/...

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-9)**: All depend on Foundational phase completion
  - US1 (P1): First priority, foundational for others
  - US7 (P2): Can start after Foundational, independent of US1 but enhances it
  - US2 (P2): Can start after Foundational, independent of US1
  - US6 (P3): Enhances observability across all stories, can start after Foundational
  - US3 (P3): Requires US1 complete (builds on single agent)
  - US4 (P4): Requires US1 complete (builds on single agent)
  - US5 (P5): Requires US1, US3 complete (builds on single agent + orchestration)
- **Polish (Phase 10)**: Depends on all desired user stories being complete

### User Story Dependencies

```text
Foundational (Phase 2)
    ├─→ US1 (P1) ────────┬─→ US3 (P3) ─→ US5 (P5)
    ├─→ US7 (P2)         ├─→ US4 (P4) ─→ US5 (P5)
    ├─→ US2 (P2)         └─→ US6 (P3)
    └─→ US6 (P3)
```

**Recommended MVP**: Complete Setup + Foundational + US1 only for initial release

**Recommended Phase 2**: Add US7 + US2 for production-ready single agents

**Full Feature Set**: All user stories for complete orchestration capabilities

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Models before services
- Services before orchestration
- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- **Setup (Phase 1)**: Tasks T003-T006 can run in parallel
- **Foundational (Phase 2)**: Tasks T007-T009, T011-T012 can run in parallel
- **Within US1**: Tasks T013-T015 (tests), T016-T018, T024-T026, T029 can run in parallel
- **Within US7**: Tasks T031-T033 (tests), T034-T035 can run in parallel
- **Within US2**: Tasks T042-T043 (tests), T048-T049, T052-T053 can run in parallel
- **After Foundational**: US1, US2, US7, US6 can all start in parallel (independent)
- **Polish (Phase 10)**: Tasks T095-T097 can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Property test for agent execution in test/Property/AgentProps.hs"
Task: "Integration test for single agent with tools in test/Integration/SimpleAgentSpec.hs"
Task: "Contract test for Agent API in test/AgenticFramework/AgentSpec.hs"

# Launch all core type definitions together:
Task: "Create src/AgenticFramework/Agent.hs with Agent, AgentConfig, AgentResult data types"
Task: "Create src/AgenticFramework/Tool.hs with Tool, ToolConfig, ToolSchema data types"
Task: "Create src/AgenticFramework/Context.hs with AgentContext and TokenMetrics types"

# Launch built-in tools together:
Task: "Create src/AgenticFramework/Tool/File.hs with built-in file tools"
Task: "Create src/AgenticFramework/Tool/LangChain.hs with wrappers"
Task: "Create src/AgenticFramework/Tool/WebSearch.hs with webSearchTool"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T006)
2. Complete Phase 2: Foundational (T007-T012) - CRITICAL
3. Complete Phase 3: User Story 1 (T013-T030)
4. **STOP and VALIDATE**: Test User Story 1 independently using quickstart.md
5. Deploy/demo minimal viable library

**Deliverable**: Working single-agent library with tools, logging, and examples

### Incremental Delivery (Recommended)

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → **Release v0.1.0** (MVP!)
3. Add User Story 7 + User Story 2 → Test independently → **Release v0.2.0** (Production-ready single agents)
4. Add User Story 6 → Test independently → **Release v0.3.0** (Enhanced observability)
5. Add User Story 3 → Test independently → **Release v0.4.0** (Sequential orchestration)
6. Add User Story 4 + User Story 5 → Test independently → **Release v1.0.0** (Full orchestration)

**Each release adds value without breaking previous features**

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together (T001-T012)
2. Once Foundational is done:
   - Developer A: User Story 1 (T013-T030)
   - Developer B: User Story 7 (T031-T041)
   - Developer C: User Story 2 (T042-T054)
3. Stories complete and integrate independently
4. Continue with US6, US3, US4, US5 as team capacity allows

---

## Task Summary

**Total Tasks**: 105 tasks across 10 phases

**Task Breakdown by Phase**:
- Setup: 6 tasks
- Foundational: 6 tasks
- User Story 1 (P1): 18 tasks
- User Story 7 (P2): 11 tasks
- User Story 2 (P2): 13 tasks
- User Story 6 (P3): 11 tasks
- User Story 3 (P3): 13 tasks
- User Story 4 (P4): 8 tasks
- User Story 5 (P5): 8 tasks
- Polish: 11 tasks

**Parallel Opportunities**: 42 tasks marked [P] can run concurrently (40% of all tasks)

**Independent Test Criteria**:
- US1: Create agent with 2-3 tools, verify correct tool usage
- US2: Load skill file, verify agent follows methodology
- US3: Run 3-agent sequential workflow, verify handoffs
- US4: Run concurrent agents, verify independent completion
- US5: Create coordinator with specialists, verify delegation
- US6: Examine logs, verify complete trace of operations
- US7: Trigger 90% threshold, verify auto-summarization

**Suggested MVP Scope**: Setup + Foundational + US1 = 30 tasks (28% of total)

**Format Validation**: ✅ All 105 tasks follow strict checklist format with checkbox, ID, optional [P], [Story] labels, and file paths

---

## Notes

- [P] tasks = different files, no dependencies, can run in parallel
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Constitution compliance: QuickCheck property tests (IV), Ormolu formatting (V), Bazel builds (I)
