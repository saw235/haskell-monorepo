---

description: "Task list for typed agent workflows feature implementation"
---

# Tasks: Typed Agent Workflows

**Input**: Design documents from `/specs/005-typed-agent-workflows/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Including test tasks as specified in plan.md (QuickCheck properties and HSpec unit tests)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description (FR-XXX)`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- **FR-XXX**: Functional requirement(s) this task implements (REQUIRED per Constitution VI)
- Include exact file paths in descriptions

## Path Conventions

- Library code: `haskell/libs/agentic-framework/src/AgenticFramework/`
- Tests: `haskell/libs/agentic-framework/test/`
- Examples: `haskell/libs/agentic-framework/examples/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [x] T001 Create Workflow module directories in haskell/libs/agentic-framework/src/AgenticFramework/ (Infrastructure)
- [x] T002 Update BUILD.bazel to include new dependencies (mtl, transformers already present) (Infrastructure)
- [x] T003 [P] Create test directory structure in haskell/libs/agentic-framework/test/Workflow/ (Infrastructure)
- [x] T004 [P] Setup example directory structure in haskell/libs/agentic-framework/examples/ (Infrastructure)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T005 Define core workflow types in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Types.hs (FR-002, FR-006)
- [x] T006 Create AgentContext type in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Types.hs (FR-003, FR-009)
- [x] T007 Define WorkflowError type in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Types.hs (FR-011)
- [x] T008 Create Workflow monad definition in haskell/libs/agentic-framework/src/AgenticFramework/Workflow.hs (FR-002, FR-006)
- [x] T009 Implement basic monad instances (Functor, Applicative, Monad) in haskell/libs/agentic-framework/src/AgenticFramework/Workflow.hs (FR-002)
- [x] T010 Create Capability base type in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Types.hs (FR-001, FR-007)

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Define Agent with Natural Language Capabilities (Priority: P1) 🎯 MVP

**Goal**: Allow developers to create agents with natural language capabilities without complex configuration

**Independent Test**: Can create an agent with capabilities and verify it responds according to defined behaviors

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T011 [P] [US1] QuickCheck property test for capability composition in haskell/libs/agentic-framework/test/Workflow/CapabilitySpec.hs (FR-001, FR-007, SC-001)
- [x] T012 [P] [US1] HSpec unit test for agent creation with capabilities in haskell/libs/agentic-framework/test/Workflow/CapabilitySpec.hs (FR-001, SC-001)
- [x] T013 [P] [US1] Test capability application to prompts in haskell/libs/agentic-framework/test/Workflow/CapabilitySpec.hs (FR-001, FR-007)

### Implementation for User Story 1

- [x] T014 [P] [US1] Implement Capability data type with natural language fields in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Capabilities.hs (FR-001)
- [x] T015 [P] [US1] Create capability modifier function (Prompt -> Prompt) in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Capabilities.hs (FR-001)
- [x] T016 [US1] Extend Agent type to include capabilities in haskell/libs/agentic-framework/src/AgenticFramework/Types.hs (FR-001, FR-003)
- [x] T017 [US1] Implement AgentBuilder monad in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Builder.hs (FR-001, FR-007)
- [x] T018 [US1] Create withCapability function in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Builder.hs (FR-001, FR-007)
- [x] T019 [US1] Implement buildAgent function in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Builder.hs (FR-001, FR-003)
- [x] T020 [US1] Create simple example in haskell/libs/agentic-framework/examples/SimpleAgent.hs (FR-001)

**Checkpoint**: User Story 1 complete - developers can create agents with natural language capabilities

---

## Phase 4: User Story 2 - Compose Typed Workflows with Data Flow (Priority: P2)

**Goal**: Enable creation of structured workflows with typed data flow between steps

**Independent Test**: Can create a multi-step workflow and verify data flows correctly with type safety

### Tests for User Story 2

- [x] T021 [P] [US2] QuickCheck property test for workflow type safety in haskell/libs/agentic-framework/test/Workflow/TypeSafetySpec.hs (FR-002, FR-006, SC-003)
- [x] T022 [P] [US2] Test workflow composition in haskell/libs/agentic-framework/test/Workflow/CompositionSpec.hs (FR-002, FR-010)
- [x] T023 [P] [US2] Test data flow between steps in haskell/libs/agentic-framework/test/Workflow/CompositionSpec.hs (FR-002, FR-006)

### Implementation for User Story 2

- [x] T024 [P] [US2] Implement indexed monad for workflow phases in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Indexed.hs (FR-002, FR-006)
- [x] T025 [P] [US2] Create workflow DSL functions (llmCall, useTool) in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/DSL.hs (FR-002, FR-008)
- [x] T026 [US2] Implement workflow execution engine in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Execution.hs (FR-002, FR-009)
- [x] T027 [US2] Add conditional branching support in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/DSL.hs (FR-010)
- [x] T028 [US2] Implement parallel step execution in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Execution.hs (FR-014)
- [x] T029 [US2] Create workflow validation function in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Validation.hs (FR-013)
- [x] T030 [US2] Add workflow to Agent type in haskell/libs/agentic-framework/src/AgenticFramework/Types.hs (FR-002, FR-003)
- [x] T031 [US2] Create SimpleWorkflow example in haskell/libs/agentic-framework/examples/SimpleWorkflow.hs (FR-002)

**Checkpoint**: User Story 2 complete - developers can create typed workflows with data flow

---

## Phase 5: User Story 3 - Apply Capabilities to Workflow Steps (Priority: P2)

**Goal**: Allow fine-grained control by applying capabilities to individual workflow steps

**Independent Test**: Can create workflow with different capabilities per step and verify behavior

### Tests for User Story 3

- [x] T032 [P] [US3] Test step-level capability application in haskell/libs/agentic-framework/test/Workflow/CapabilitySpec.hs (FR-004, SC-007)
- [x] T033 [P] [US3] Test capability isolation between steps in haskell/libs/agentic-framework/test/Workflow/CapabilitySpec.hs (FR-004)

### Implementation for User Story 3

- [x] T034 [US3] Implement withCapability for workflow steps in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/DSL.hs (FR-004)
- [x] T035 [US3] Create local capability context modification in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Execution.hs (FR-004)
- [x] T036 [US3] Add capability tracking to WorkflowState in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Types.hs (FR-004, FR-009)
- [x] T037 [US3] Implement capability stacking/composition in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Capabilities.hs (FR-004, FR-007)
- [x] T038 [US3] Create ResearchAgent example with step capabilities in haskell/libs/agentic-framework/examples/ResearchAgent.hs (FR-004)

**Checkpoint**: User Story 3 complete - capabilities can be applied to individual workflow steps

---

## Phase 6: User Story 4 - Execute Agents with or without Workflows (Priority: P3)

**Goal**: Provide flexibility with both workflow-based and traditional ReAct execution modes

**Independent Test**: Can execute same agent in both modes and verify correct behavior

### Tests for User Story 4

- [x] T039 [P] [US4] Test workflow execution mode in haskell/libs/agentic-framework/test/Integration/ExecutionModeSpec.hs (FR-005, SC-004)
- [x] T040 [P] [US4] Test traditional ReAct execution mode in haskell/libs/agentic-framework/test/Integration/ExecutionModeSpec.hs (FR-005)
- [x] T041 [P] [US4] Test mode selection logic in haskell/libs/agentic-framework/test/Integration/ExecutionModeSpec.hs (FR-005)

### Implementation for User Story 4

- [x] T042 [US4] Modify executeAgent to support optional workflows in haskell/libs/agentic-framework/src/AgenticFramework/Agent.hs (FR-005)
- [x] T043 [US4] runWorkflow function already exists in haskell/libs/agentic-framework/src/AgenticFramework/Workflow.hs (FR-005, FR-008)
- [x] T044 [US4] Create traditionalExecution function (executeTraditional) in haskell/libs/agentic-framework/src/AgenticFramework/Agent.hs (FR-005)
- [x] T045 [US4] Add ExecutionMode type and selection in haskell/libs/agentic-framework/src/AgenticFramework/Types.hs (FR-005)
- [x] T046 [US4] Tool invocation in workflows already implemented in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/DSL.hs (FR-008)
- [x] T047 [US4] Add context preservation across steps in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Execution.hs (FR-009)
- [x] T048 [US4] Create DebugAgent example showing both modes in haskell/libs/agentic-framework/examples/DebugAgent.hs (FR-005)

**Checkpoint**: User Story 4 complete - agents can execute in either workflow or traditional mode

---

## Phase 7: User Story 5 - Load and Compose Agent Capabilities Dynamically (Priority: P4)

**Goal**: Enable runtime loading of capabilities from configuration files

**Independent Test**: Can load capabilities from JSON files and verify agent behavior changes

### Tests for User Story 5

- [x] T049 [P] [US5] Test JSON capability loading in haskell/libs/agentic-framework/test/Workflow/CapabilitySpec.hs (FR-015, SC-005)
- [x] T050 [P] [US5] Test invalid capability handling in haskell/libs/agentic-framework/test/Workflow/CapabilitySpec.hs (FR-015, FR-011)

### Implementation for User Story 5

- [x] T051 [P] [US5] Create JSON schema for capabilities in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Schema.hs (FR-015)
- [x] T052 [P] [US5] Implement loadCapability from file in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Loader.hs (FR-015)
- [x] T053 [US5] Add capability caching with TTL in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Cache.hs (FR-015)
- [x] T054 [US5] Create loadCapabilities for directory in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Loader.hs (FR-015)
- [x] T055 [US5] Implement capability validation in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Validation.hs (FR-015, FR-011)
- [x] T056 [US5] Add runtime capability composition in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Capabilities.hs (FR-015, FR-007)
- [x] T057 [US5] Create example capability JSON files in haskell/libs/agentic-framework/examples/capabilities/ (FR-015)

**Checkpoint**: User Story 5 complete - capabilities can be loaded and composed at runtime

---

## Phase 8: Advanced Features & Integration

**Purpose**: Cross-cutting features that enhance multiple user stories

- [x] T058 Implement nested workflow support in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Execution.hs (FR-012)
- [x] T059 Add comprehensive error messages in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Errors.hs (FR-011, SC-007)
- [x] T060 Create LangChain tool abstraction layer in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Tools.hs (FR-008)
- [x] T061 [P] Add performance monitoring in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Metrics.hs (SC-002, SC-004)
- [x] T062 [P] Implement concurrent agent execution support in haskell/libs/agentic-framework/src/AgenticFramework/Workflow/Concurrent.hs (SC-008)

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Final improvements and documentation

- [ ] T063 [P] Update README with workflow usage in haskell/libs/agentic-framework/README.md (Documentation)
- [ ] T064 [P] Add Haddock documentation to all public functions (Documentation)
- [ ] T065 Run ormolu formatting on all new Haskell files (Code Quality)
- [ ] T066 [P] Create comprehensive integration tests in haskell/libs/agentic-framework/test/IntegrationSpec.hs (SC-003, SC-006)
- [ ] T067 [P] Performance benchmarks in haskell/libs/agentic-framework/bench/WorkflowBench.hs (SC-002, SC-004, SC-005)
- [ ] T068 Validate quickstart.md examples work correctly (Documentation)
- [ ] T069 [P] Add migration guide for existing users in haskell/libs/agentic-framework/MIGRATION.md (Documentation)
- [ ] T070 Final BUILD.bazel cleanup and optimization (Infrastructure)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - US1 (P1) should complete first as MVP
  - US2 and US3 (both P2) can proceed in parallel after US1
  - US4 (P3) can start after Foundational but benefits from US2 completion
  - US5 (P4) can start after US1 completion
- **Advanced Features (Phase 8)**: Can start after US2 and US3
- **Polish (Phase 9)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: No dependencies - true MVP
- **User Story 2 (P2)**: Independent but builds on US1's capability system
- **User Story 3 (P2)**: Depends on US2 (needs workflows) and US1 (needs capabilities)
- **User Story 4 (P3)**: Benefits from US2 completion but can start independently
- **User Story 5 (P4)**: Extends US1's capability system

### Parallel Opportunities

- Setup tasks T003-T004 can run in parallel
- All tests within a story can run in parallel (marked [P])
- US2 and US3 can be developed in parallel after US1
- Polish tasks T063-T067, T069 can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "QuickCheck property test for capability composition"
Task: "HSpec unit test for agent creation with capabilities"
Task: "Test capability application to prompts"

# Launch capability implementation tasks together:
Task: "Implement Capability data type"
Task: "Create capability modifier function"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test agent creation with capabilities
5. Release v0.1.0 with basic capability support

### Incremental Delivery

1. v0.1.0: Setup + Foundational + US1 → Basic capabilities
2. v0.2.0: Add US2 → Typed workflows
3. v0.3.0: Add US3 → Step-level capabilities
4. v0.4.0: Add US4 → Dual execution modes
5. v1.0.0: Add US5 + Polish → Full feature set

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Developer A: US1 (MVP) → US4 (execution modes)
3. Developer B: US2 (workflows) → US3 (step capabilities)
4. Developer C: US5 (dynamic loading) + Advanced Features
5. All: Polish phase together

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- (FR-XXX) tags link task to functional requirements from spec.md (REQUIRED per Constitution VI)
- Each user story should be independently completable and testable
- Tests use QuickCheck for properties, HSpec for unit tests
- Follow Bazel build system requirements throughout
- Use ormolu for formatting before commits
- Commit after each task or logical group