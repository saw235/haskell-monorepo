# Tasks: Haskell Rich Terminal Library Enhancements

**Input**: Design documents from `/specs/003-haskell-rich-library/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/api-contracts.md

**Tests**: QuickCheck property-based tests and unit tests are required per Constitution Principle IV.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

This is a Haskell library enhancement at `haskell/libs/rich/` with tests in `haskell/libs/rich/test/`.

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and dependency setup

- [ ] T001 Add terminal-size to MODULE.bazel _SIMPLE_PACKAGES list
- [ ] T002 Add wcwidth to MODULE.bazel _SIMPLE_PACKAGES list
- [ ] T003 Run bazel run @stackage-unpinned//:pin -- --upgrade-hackage to pin new dependencies
- [ ] T004 Update haskell/libs/rich/BUILD.bazel deps to include //:terminal-size, //:wcwidth, //:ansi-terminal
- [ ] T005 Verify Bazel build with bazel build //haskell/libs/rich

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core terminal detection modules that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T006 [P] Create Rich.Terminal.Size module in haskell/libs/rich/src/Rich/Terminal/Size.hs (Terminal size detection wrapper)
- [ ] T007 [P] Create Rich.Terminal.Width module in haskell/libs/rich/src/Rich/Terminal/Width.hs (Unicode width calculation wrapper)
- [ ] T008 [P] Create Rich.Terminal.Capability module in haskell/libs/rich/src/Rich/Terminal/Capability.hs (Color capability detection wrapper)
- [ ] T009 Create Rich.Terminal module in haskell/libs/rich/src/Rich/Terminal.hs (Re-exports all Terminal.* modules)
- [ ] T010 Update haskell/libs/rich/src/Rich.hs to re-export Rich.Terminal module
- [ ] T011 Update haskell/libs/rich/BUILD.bazel srcs to include all new Terminal modules

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Display Styled Terminal Text (Priority: P1) 🎯 MVP

**Goal**: Core styling functionality works correctly with terminal capability detection and graceful degradation

**Independent Test**: Create styles with various colors/attributes, detect terminal capability, render with appropriate degradation, verify ANSI codes are correct

**Why P1**: Core functionality that all other features depend on. Without styled text output, the library provides no value.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T012 [P] [US1] Create QuickCheck properties for Terminal.Capability in haskell/libs/rich/test/Rich/Terminal/CapabilitySpec.hs
- [ ] T013 [P] [US1] Create unit tests for color degradation logic in haskell/libs/rich/test/Rich/Terminal/CapabilitySpec.hs
- [ ] T014 [P] [US1] Add test cases for TERM environment variable detection in haskell/libs/rich/test/Rich/Terminal/CapabilitySpec.hs

### Implementation for User Story 1

- [ ] T015 [US1] Implement detectColorCapability function in haskell/libs/rich/src/Rich/Terminal/Capability.hs
- [ ] T016 [US1] Implement detectColorCapabilityFor Handle variant in haskell/libs/rich/src/Rich/Terminal/Capability.hs
- [ ] T017 [US1] Implement supportsANSI helper in haskell/libs/rich/src/Rich/Terminal/Capability.hs
- [ ] T018 [US1] Add color degradation logic to Rich.Style rendering in haskell/libs/rich/src/Rich/Style.hs
- [ ] T019 [US1] Update Console printing functions to use capability detection in haskell/libs/rich/src/Rich/Console.hs
- [ ] T020 [US1] Run tests and verify all US1 tests pass

**Checkpoint**: At this point, User Story 1 should be fully functional - styled text with automatic color degradation works

---

## Phase 4: User Story 2 - Create Formatted Tables (Priority: P2)

**Goal**: Tables display with responsive rendering, automatic column sizing within terminal width bounds, and multi-line cell support

**Independent Test**: Create tables with headers and rows, detect terminal width, verify table fits within bounds with proper wrapping/truncation, test multi-line cells expand row height correctly

**Why P2**: Tables are common requirement for CLI tools. Independent of other features except US1 (styling).

### Tests for User Story 2

- [ ] T021 [P] [US2] Create QuickCheck properties for Unicode width calculation in haskell/libs/rich/test/Rich/Terminal/WidthSpec.hs
- [ ] T022 [P] [US2] Create unit tests for CJK character width in haskell/libs/rich/test/Rich/Terminal/WidthSpec.hs
- [ ] T023 [P] [US2] Create unit tests for emoji width in haskell/libs/rich/test/Rich/Terminal/WidthSpec.hs
- [ ] T024 [P] [US2] Create unit tests for combining character width in haskell/libs/rich/test/Rich/Terminal/WidthSpec.hs
- [ ] T025 [P] [US2] Create terminal size detection tests in haskell/libs/rich/test/Rich/Terminal/SizeSpec.hs
- [ ] T026 [P] [US2] Create multi-line cell rendering tests in haskell/libs/rich/test/Rich/MultiLineSpec.hs
- [ ] T027 [P] [US2] Create responsive table rendering tests in haskell/libs/rich/test/Rich/TableSpec.hs

### Implementation for User Story 2

- [ ] T028 [P] [US2] Implement displayWidth function in haskell/libs/rich/src/Rich/Terminal/Width.hs
- [ ] T029 [P] [US2] Implement safeDisplayWidth function in haskell/libs/rich/src/Rich/Terminal/Width.hs
- [ ] T030 [P] [US2] Implement displayWidthIgnoringANSI function in haskell/libs/rich/src/Rich/Terminal/Width.hs
- [ ] T031 [P] [US2] Implement getTerminalSize function in haskell/libs/rich/src/Rich/Terminal/Size.hs
- [ ] T032 [P] [US2] Implement getTerminalWidth function in haskell/libs/rich/src/Rich/Terminal/Size.hs
- [ ] T033 [P] [US2] Implement getTerminalHeight function in haskell/libs/rich/src/Rich/Terminal/Size.hs
- [ ] T034 [US2] Create Rich.Internal.MultiLine module in haskell/libs/rich/src/Rich/Internal/MultiLine.hs
- [ ] T035 [US2] Implement splitCell function for multi-line cells in haskell/libs/rich/src/Rich/Internal/MultiLine.hs
- [ ] T036 [US2] Implement calculateRowHeight function in haskell/libs/rich/src/Rich/Internal/MultiLine.hs
- [ ] T037 [US2] Implement renderCellWithHeight function in haskell/libs/rich/src/Rich/Internal/MultiLine.hs
- [ ] T038 [US2] Update Table rendering to use displayWidth for column width calculation in haskell/libs/rich/src/Rich/Table.hs
- [ ] T039 [US2] Implement multi-line cell support in table row rendering in haskell/libs/rich/src/Rich/Table.hs
- [ ] T040 [US2] Implement renderTableWithWidth function in haskell/libs/rich/src/Rich/Table.hs
- [ ] T041 [US2] Implement renderTableResponsive function in haskell/libs/rich/src/Rich/Table.hs
- [ ] T042 [US2] Add column shrinking logic for tables exceeding terminal width in haskell/libs/rich/src/Rich/Table.hs
- [ ] T043 [US2] Update BUILD.bazel srcs to include Rich/Internal/MultiLine.hs
- [ ] T044 [US2] Run tests and verify all US2 tests pass

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently - styled text works, and responsive tables with multi-line cells work

---

## Phase 5: User Story 3 - Display Content in Panels (Priority: P2)

**Goal**: Panels render with responsive width adjustment, proper padding, and support for multi-line content

**Independent Test**: Create panels with various configurations (title, subtitle, padding, border styles), detect terminal width, verify panel fits within bounds with proper content wrapping

**Why P2**: Panels provide visual emphasis for important messages. Independent of tables but uses same responsive rendering logic.

### Tests for User Story 3

- [ ] T045 [P] [US3] Create panel rendering tests in haskell/libs/rich/test/Rich/PanelSpec.hs for multi-line content
- [ ] T046 [P] [US3] Create responsive panel rendering tests in haskell/libs/rich/test/Rich/PanelSpec.hs

### Implementation for User Story 3

- [ ] T047 [US3] Update Panel rendering to use displayWidth for content width calculation in haskell/libs/rich/src/Rich/Panel.hs
- [ ] T048 [US3] Implement renderPanelWithWidth function in haskell/libs/rich/src/Rich/Panel.hs
- [ ] T049 [US3] Implement renderPanelResponsive function in haskell/libs/rich/src/Rich/Panel.hs
- [ ] T050 [US3] Add content wrapping logic for panels exceeding terminal width in haskell/libs/rich/src/Rich/Panel.hs
- [ ] T051 [US3] Run tests and verify all US3 tests pass

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently - styling, responsive tables, and responsive panels all work

---

## Phase 6: User Story 4 - Visualize Hierarchical Data as Trees (Priority: P3)

**Goal**: Trees display hierarchical structures with correct indentation and Unicode width handling for labels

**Independent Test**: Build tree structures with nodes and children, render with Unicode labels (CJK, emoji), verify proper indentation and connecting lines with correct visual alignment

**Why P3**: Trees are useful for specific use cases but less universally needed. Can be developed independently after core styling works.

### Tests for User Story 4

- [ ] T052 [P] [US4] Create tree rendering tests with Unicode labels in haskell/libs/rich/test/Rich/TreeSpec.hs

### Implementation for User Story 4

- [ ] T053 [US4] Update Tree rendering to use displayWidth for label alignment in haskell/libs/rich/src/Rich/Tree.hs
- [ ] T054 [US4] Fix tree guide character alignment with wide characters in haskell/libs/rich/src/Rich/Tree.hs
- [ ] T055 [US4] Run tests and verify all US4 tests pass

**Checkpoint**: All user stories 1-4 should now be independently functional

---

## Phase 7: User Story 5 - Show Progress with Animated Bars (Priority: P3)

**Goal**: Progress bars display correctly with Unicode width handling for description text and custom characters

**Independent Test**: Create progress bars with various current/total values and Unicode descriptions, render at different completion percentages, verify visual representation is correct even with wide characters

**Why P3**: Progress bars enhance UX for time-consuming tasks but not core. Independent feature focused on visual feedback.

### Tests for User Story 5

- [ ] T056 [P] [US5] Create progress bar rendering tests with Unicode descriptions in haskell/libs/rich/test/Rich/ProgressSpec.hs

### Implementation for User Story 5

- [ ] T057 [US5] Update Progress bar rendering to use displayWidth for description width in haskell/libs/rich/src/Rich/Progress.hs
- [ ] T058 [US5] Fix progress bar alignment with wide character descriptions in haskell/libs/rich/src/Rich/Progress.hs
- [ ] T059 [US5] Run tests and verify all US5 tests pass

**Checkpoint**: All user stories should now be independently functional

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and final validation

- [ ] T060 [P] Add Haddock documentation for all new Terminal.* modules in haskell/libs/rich/src/Rich/Terminal/*.hs
- [ ] T061 [P] Update README.md with new features and usage examples at haskell/libs/rich/README.md
- [ ] T062 [P] Create demo application showing all new features at haskell/app/rich-demo-enhanced/Main.hs
- [ ] T063 Run all tests with bazel test //haskell/libs/rich/...
- [ ] T064 Run code formatting with ormolu on all new .hs files
- [ ] T065 Run buildifier on BUILD.bazel files
- [ ] T066 Validate quickstart.md examples by compiling and running them
- [ ] T067 Verify backward compatibility by running existing apps that use Rich library
- [ ] T068 Performance testing: render 100-row tables and verify <50ms target
- [ ] T069 Cross-platform testing: verify builds and tests pass on Linux (primary), macOS and Windows (if available)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2 → P2 → P3 → P3)
- **Polish (Phase 8)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational (Phase 2) - Uses US1 styling but independently testable
- **User Story 3 (P2)**: Can start after Foundational (Phase 2) - Uses US1 styling and US2 width logic, but independently testable
- **User Story 4 (P3)**: Can start after Foundational (Phase 2) - Uses US1 styling and US2 width logic, but independently testable
- **User Story 5 (P3)**: Can start after Foundational (Phase 2) - Uses US1 styling and US2 width logic, but independently testable

**Note**: While US2-US5 use width/capability detection from Foundational phase, they should each deliver independent value. A user could use just styling (US1), or styling + tables (US1+US2), etc.

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD approach per Constitution)
- Terminal detection functions before rendering logic that uses them
- Multi-line cell logic before table rendering modifications
- Core implementation before integration tests
- Story complete and verified before moving to next priority

### Parallel Opportunities

- **Setup (Phase 1)**: T001 and T002 can run in parallel (different files in MODULE.bazel)
- **Foundational (Phase 2)**: T006, T007, T008 can all run in parallel (different files)
- **Once Foundational completes**: All user stories can start in parallel (if team capacity allows)
- **Within each story**: All test creation tasks marked [P] can run in parallel
- **Within US2**: T028-T033 (Terminal.Width and Terminal.Size functions) can run in parallel
- **Different user stories** can be worked on in parallel by different team members

---

## Parallel Example: User Story 2 (Tables)

```bash
# Launch all Terminal.* function implementations together:
Task: "Implement displayWidth function in haskell/libs/rich/src/Rich/Terminal/Width.hs"
Task: "Implement safeDisplayWidth function in haskell/libs/rich/src/Rich/Terminal/Width.hs"
Task: "Implement displayWidthIgnoringANSI function in haskell/libs/rich/src/Rich/Terminal/Width.hs"
Task: "Implement getTerminalSize function in haskell/libs/rich/src/Rich/Terminal/Size.hs"
Task: "Implement getTerminalWidth function in haskell/libs/rich/src/Rich/Terminal/Size.hs"
Task: "Implement getTerminalHeight function in haskell/libs/rich/src/Rich/Terminal/Size.hs"

# Launch all test creation tasks together:
Task: "Create QuickCheck properties for Unicode width calculation in test/Rich/Terminal/WidthSpec.hs"
Task: "Create unit tests for CJK character width in test/Rich/Terminal/WidthSpec.hs"
Task: "Create unit tests for emoji width in test/Rich/Terminal/WidthSpec.hs"
Task: "Create terminal size detection tests in test/Rich/Terminal/SizeSpec.hs"
Task: "Create multi-line cell rendering tests in test/Rich/MultiLineSpec.hs"
```

---

## Parallel Example: Multiple User Stories

```bash
# After Foundational phase completes, three developers can work in parallel:

Developer A (User Story 1 - Capability Detection):
  - T012-T020: Terminal capability detection and color degradation

Developer B (User Story 2 - Responsive Tables):
  - T021-T044: Terminal size/width detection, multi-line cells, responsive tables

Developer C (User Story 3 - Responsive Panels):
  - T045-T051: Responsive panel rendering
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T005)
2. Complete Phase 2: Foundational (T006-T011) - CRITICAL
3. Complete Phase 3: User Story 1 (T012-T020)
4. **STOP and VALIDATE**: Test styled text with color degradation independently
5. Deploy/demo if ready - basic styling with capability detection works

**MVP Value**: Library now gracefully degrades colors based on terminal capability, improving compatibility across different terminal emulators.

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready (T001-T011)
2. Add User Story 1 → Test independently → Deploy/Demo (MVP - styling with degradation!)
3. Add User Story 2 → Test independently → Deploy/Demo (MVP + responsive tables!)
4. Add User Story 3 → Test independently → Deploy/Demo (MVP + tables + panels!)
5. Add User Story 4 → Test independently → Deploy/Demo (MVP + tables + panels + trees!)
6. Add User Story 5 → Test independently → Deploy/Demo (full feature set!)
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together (T001-T011)
2. Once Foundational is done:
   - Developer A: User Story 1 (T012-T020)
   - Developer B: User Story 2 (T021-T044)
   - Developer C: User Story 3 (T045-T051)
3. Stories complete and integrate independently
4. Developers D & E can handle US4 and US5 in parallel

**Recommended Priority**: US1 → US2 → US3 → US4 → US5 (matches P1, P2, P2, P3, P3 priorities from spec)

---

## Notes

- [P] tasks = different files, no dependencies between them
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Tests must fail before implementing (TDD per Constitution Principle IV)
- Run `bazel test //haskell/libs/rich/...` after each story to verify
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Use `bazel run @stackage-unpinned//:pin` after adding new packages to MODULE.bazel
- Format code with `ormolu --mode inplace` before committing
- All modules must have Haddock documentation
- Maintain backward compatibility - existing Rich library users should not break

---

## Task Count Summary

- **Phase 1 (Setup)**: 5 tasks
- **Phase 2 (Foundational)**: 6 tasks
- **Phase 3 (US1 - Styling)**: 9 tasks (3 test tasks + 6 implementation tasks)
- **Phase 4 (US2 - Tables)**: 24 tasks (7 test tasks + 17 implementation tasks)
- **Phase 5 (US3 - Panels)**: 7 tasks (2 test tasks + 5 implementation tasks)
- **Phase 6 (US4 - Trees)**: 4 tasks (1 test task + 3 implementation tasks)
- **Phase 7 (US5 - Progress)**: 4 tasks (1 test task + 3 implementation tasks)
- **Phase 8 (Polish)**: 10 tasks

**Total**: 69 tasks

**Parallel Opportunities**: 29 tasks marked [P] can run in parallel within their phases

**Independent Test Criteria**:
- US1: Styles render with correct ANSI codes based on detected capability
- US2: Tables fit terminal width, multi-line cells expand row height correctly
- US3: Panels fit terminal width with proper content wrapping
- US4: Trees align correctly with Unicode labels
- US5: Progress bars display correctly with Unicode descriptions

**Suggested MVP Scope**: Phase 1 + Phase 2 + Phase 3 (User Story 1 only) = 20 tasks
