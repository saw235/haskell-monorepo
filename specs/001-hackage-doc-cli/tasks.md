# Tasks: Hackage Documentation CLI Query Tool

**Input**: Design documents from `/home/xsaw/haskell-monorepo/specs/001-hackage-doc-cli/`
**Prerequisites**: plan.md (complete), spec.md (complete), research.md (complete), data-model.md (complete), contracts/ (complete)

**Tests**: Per constitution requirement, tests are INCLUDED with local fixtures (no live Hackage requests)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2)
- Include exact file paths in descriptions

## Path Conventions

Per plan.md structure:
- **Library**: `haskell/libs/hackage-client/`
- **CLI App**: `haskell/app/hackage-doc-cli/`
- **Tests**: `haskell/app/hackage-doc-cli/test/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic directory structure

- [X] T001 Create library directory structure at haskell/libs/hackage-client/HackageClient
- [X] T002 Create CLI app directory structure at haskell/app/hackage-doc-cli/CLI
- [X] T003 Create test directory structure at haskell/app/hackage-doc-cli/test/Fixtures
- [X] T004 Create BUILD.bazel for hackage-client library with dependencies (base, aeson, bytestring, containers, directory, filepath, http-conduit, text, time)
- [X] T005 Create BUILD.bazel for hackage-doc-cli app with dependencies (base, optparse-applicative, text, //haskell/libs/hackage-client)
- [X] T006 Create BUILD.bazel for tests with Tasty, HUnit, QuickCheck dependencies

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data types and infrastructure that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T007 [P] Implement Package data type with JSON instances in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T008 [P] Implement Version data type with JSON instances in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T009 [P] Implement Module data type with JSON instances in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T010 [P] Implement Function data type with JSON instances in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T011 [P] Implement Type data type (with Constructor, RecordField, TypeKind) with JSON instances in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T012 [P] Implement TypeClass data type (with Method) with JSON instances in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T013 [P] Implement Dependency data type with JSON instances in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T014 [P] Implement QueryResult data type with JSON instances in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T015 [P] Implement CacheEntry data type with JSON instances and isCacheValid function in haskell/libs/hackage-client/HackageClient/Types.hs
- [X] T016 Create test fixtures from real Hackage responses in haskell/app/hackage-doc-cli/test/Fixtures/ (aeson-package.json, aeson-versions.json, aeson-module.html)
- [X] T017 Implement cache directory setup (cacheDir) in haskell/libs/hackage-client/HackageClient/Cache.hs
- [X] T018 Implement cache read function (readCache) in haskell/libs/hackage-client/HackageClient/Cache.hs
- [X] T019 Implement cache write function (writeCache) with TTL support in haskell/libs/hackage-client/HackageClient/Cache.hs
- [X] T020 Implement basic HTTP request helper in haskell/libs/hackage-client/HackageClient/API.hs (using Network.HTTP.Simple)

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Quick Package Lookup (Priority: P1) 🎯 MVP

**Goal**: Display package metadata (name, version, synopsis, maintainer, doc URL) within 2 seconds

**Independent Test**: Run `bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson` and verify it displays package info with tree structure

### Tests for User Story 1

> **NOTE: Write these tests FIRST with fixtures, ensure they PASS with local data**

- [X] T021 [P] [US1] Create QuickCheck property for Package JSON serialization round-trip in haskell/app/hackage-doc-cli/test/ParserSpec.hs
- [X] T022 [P] [US1] Create HUnit test for parsing Package from fixture aeson-package.json in haskell/app/hackage-doc-cli/test/ParserSpec.hs
- [X] T023 [P] [US1] Create HUnit test for cache write/read with Package data in haskell/app/hackage-doc-cli/test/CacheSpec.hs
- [X] T024 [P] [US1] Create HUnit test for cache TTL expiration logic in haskell/app/hackage-doc-cli/test/CacheSpec.hs

### Implementation for User Story 1

- [X] T025 [US1] Implement fetchPackageInfo function to query Hackage API for package metadata in haskell/libs/hackage-client/HackageClient/API.hs
- [X] T026 [US1] Implement parsePackageResponse to parse Hackage JSON response into Package type in haskell/libs/hackage-client/HackageClient/Parser.hs
- [X] T027 [US1] Implement queryPackageWithCache function (check cache, fetch if miss, write cache) in haskell/libs/hackage-client/HackageClient.hs
- [X] T028 [US1] Implement basic tree display for Package (name, version, synopsis, doc URL) in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
- [X] T029 [US1] Implement package name similarity suggestions using Levenshtein distance in haskell/libs/hackage-client/HackageClient/Parser.hs
- [X] T030 [US1] Create command-line option parser for package query in haskell/app/hackage-doc-cli/CLI/Options.hs
- [X] T031 [US1] Implement QueryPackage command handler in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [X] T032 [US1] Wire up main entry point with option parsing and command routing in haskell/app/hackage-doc-cli/Main.hs
- [X] T033 [US1] Add error handling for network failures with clear messages in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [X] T034 [US1] Add error handling for package not found with suggestions in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [X] T035 [US1] Add cache status indicator ([cached: Xh ago] or [live]) to output in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs

**Checkpoint**: User Story 1 complete - can query package, show basic tree, handle errors, use cache

---

## Phase 4: User Story 2 - List Package Versions (Priority: P1)

**Goal**: Display all available versions of a package with release dates, sorted newest-first

**Independent Test**: Run `bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --list-versions` and verify version list with dates

### Tests for User Story 2

- [X] T036 [P] [US2] Create HUnit test for parsing version list from fixture aeson-versions.json in haskell/app/hackage-doc-cli/test/ParserSpec.hs
- [X] T037 [P] [US2] Create QuickCheck property for Version sorting (newest first) in haskell/app/hackage-doc-cli/test/ParserSpec.hs

### Implementation for User Story 2

- [X] T038 [US2] Implement fetchVersionList function to query Hackage API for package versions in haskell/libs/hackage-client/HackageClient/API.hs
- [X] T039 [US2] Implement parseVersionListResponse to parse version JSON into [Version] in haskell/libs/hackage-client/HackageClient/Parser.hs
- [X] T040 [US2] Implement version sorting logic (newest-first) with isLatest/isPreferred indicators in haskell/libs/hackage-client/HackageClient/Parser.hs
- [X] T041 [US2] Implement version list display formatter in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
- [X] T042 [US2] Add --list-versions flag to CLI options parser in haskell/app/hackage-doc-cli/CLI/Options.hs
- [X] T043 [US2] Implement ListVersions command handler in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [X] T044 [US2] Add pre-release version detection logic (alpha, beta, RC) in haskell/libs/hackage-client/HackageClient/Parser.hs

**Checkpoint**: User Story 2 complete - can list versions with dates, indicators, pre-release detection

---

## Phase 5: User Story 3 - Version-Specific Documentation (Priority: P2)

**Goal**: Query package tree for specific version instead of latest

**Independent Test**: Run `bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --version 2.2.3.0` and verify version-specific tree

### Tests for User Story 3

- [X] T045 [P] [US3] Create HUnit test for version validation and parsing in haskell/app/hackage-doc-cli/test/ParserSpec.hs
- [X] T046 [P] [US3] Create test for version-specific cache keys in haskell/app/hackage-doc-cli/test/CacheSpec.hs

### Implementation for User Story 3

- [X] T047 [US3] Modify fetchPackageInfo to accept optional version parameter in haskell/libs/hackage-client/HackageClient/API.hs
- [X] T048 [US3] Implement version validation logic (semantic versioning / PVP) in haskell/libs/hackage-client/HackageClient/Parser.hs
- [X] T049 [US3] Update cache key generation to include version (tree:{pkg}-{version}) in haskell/libs/hackage-client/HackageClient/Cache.hs
- [X] T050 [US3] Add --version flag to CLI options parser in haskell/app/hackage-doc-cli/CLI/Options.hs
- [X] T051 [US3] Update QueryPackage command handler to use optional version in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [X] T052 [US3] Implement invalid version error handling with version suggestion in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [X] T053 [US3] Set default version to latest stable when --version not specified in haskell/app/hackage-doc-cli/CLI/Commands.hs

**Checkpoint**: User Story 3 complete - can query specific versions, validate versions, default to latest

---

## Phase 6: User Story 5 - Deep Module Inspection (Priority: P2)

**Goal**: Display module details with functions, types, classes, documentation, and source code

**Independent Test**: Run `bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --module Data.Aeson` and verify full module details

### Tests for User Story 5

- [X] T054 [P] [US5] Create HUnit test for parsing module HTML from fixture aeson-module.html in haskell/app/hackage-doc-cli/test/ParserSpec.hs
- [X] T055 [P] [US5] Create test for module detail tree formatting in haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs

### Implementation for User Story 5

- [X] T056 [US5] Implement fetchModuleDetails function to retrieve Haddock HTML or tarball in haskell/libs/hackage-client/HackageClient/API.hs
- [X] T057 [US5] Implement parseModuleHTML to extract functions, types, classes from Haddock HTML using scalpel in haskell/libs/hackage-client/HackageClient/Parser.hs
- [X] T058 [US5] Implement extractSourceCode to get implementation from tarball or HTML in haskell/libs/hackage-client/HackageClient/Parser.hs
- [X] T059 [US5] Implement module detail tree display (functions + signatures + docs + source) in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
- [X] T060 [US5] Implement type display formatter (constructors, record fields) in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
- [X] T061 [US5] Implement type class display formatter (methods, constraints) in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
- [X] T062 [US5] Add --module flag to CLI options parser in haskell/app/hackage-doc-cli/CLI/Options.hs
- [X] T063 [US5] Implement QueryModule command handler in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [X] T064 [US5] Add invalid module error handling with module suggestions in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [X] T065 [US5] Update cache to support module-specific keys (module:{pkg}-{ver}-{mod}) in haskell/libs/hackage-client/HackageClient/Cache.hs

### Filter Flags (FR-014) and --with-comments Flag (FR-015)

- [ ] T066 [P] [US4] Create HUnit test for --filter-functions flag in haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs
- [ ] T067 [P] [US4] Create HUnit test for --filter-types flag in haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs
- [ ] T068 [P] [US4] Create HUnit test for --filter-classes flag in haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs
- [ ] T069 [P] [US4] Create HUnit test for combined filter flags in haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs
- [ ] T070 [P] [US4] Create HUnit test for --with-comments flag showing documentation in haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs
- [ ] T071 [P] [US4] Create HUnit test for default behavior (no --with-comments) showing only signatures in haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs
- [ ] T072 [US4] Add FilterOptions data type to haskell/libs/hackage-client/HackageClient/Types.hs
- [ ] T073 [US4] Add DisplayOptions data type to haskell/libs/hackage-client/HackageClient/Types.hs
- [ ] T074 [US4] Create Filter.hs module with applyFilters function in haskell/libs/hackage-client/HackageClient/Filter.hs
- [ ] T075 [US4] Add --filter-functions, --filter-types, --filter-classes flags to CLI options parser in haskell/app/hackage-doc-cli/CLI/Options.hs
- [ ] T076 [US4] Add --with-comments flag to CLI options parser in haskell/app/hackage-doc-cli/CLI/Options.hs
- [ ] T077 [US4] Update displayModule to conditionally include Haddock docs based on DisplayOptions in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
- [ ] T078 [US4] Integrate FilterOptions into displayModule to filter entity categories in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
- [ ] T079 [US4] Wire FilterOptions and DisplayOptions through QueryModule command handler in haskell/app/hackage-doc-cli/CLI/Commands.hs
- [ ] T080 [US4] Update BUILD.bazel to include Filter.hs module in library exports

**Checkpoint**: User Story 4 complete - deep module inspection with filtering and optional comments

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Final improvements, testing, and documentation

- [ ] T081 [P] Add QuickCheck property tests for all data types JSON serialization in haskell/app/hackage-doc-cli/test/ParserSpec.hs
- [ ] T082 [P] Add HUnit tests for tree display formatting edge cases (large packages, long signatures) in haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs
- [ ] T083 [P] Add cache invalidation test (TTL expiration) in haskell/app/hackage-doc-cli/test/CacheSpec.hs
- [ ] T084 [P] Add --help flag with comprehensive usage examples including filter and comment flags in haskell/app/hackage-doc-cli/CLI/Options.hs
- [ ] T085 [P] Add --version flag to display tool version in haskell/app/hackage-doc-cli/CLI/Options.hs
- [ ] T086 Add UTF-8 encoding error handling for terminal output in haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
- [ ] T087 Format all Haskell code with Ormolu (find . -name "*.hs" -exec ormolu --mode inplace {} \;)
- [ ] T088 Format all BUILD.bazel files with Buildifier
- [ ] T089 Run all tests and verify 100% pass rate (bazel test //haskell/app/hackage-doc-cli/test:all)
- [ ] T090 Verify quickstart.md examples work as documented
- [ ] T091 Performance testing - verify <2s queries with filtering enabled

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 (P1) + US2 (P1): Can proceed in parallel after Foundational
  - US3 (P2): Can start after Foundational, integrates with US1 for version-specific queries
  - US4 (P2): Can start after Foundational, includes filter and comment flags
- **Polish (Phase 7)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - Core package query and tree display
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - Independent version listing feature
- **User Story 3 (P2)**: Can start after Foundational, integrates with US1 for version-specific queries
- **User Story 4 (P2)**: Can start after Foundational, includes deep module inspection with filter flags and --with-comments

### Within Each User Story

- Tests written FIRST with fixtures (no live requests)
- Data types from Foundational phase
- API functions before cache integration
- Cache integration before command handlers
- Command handlers before main wiring
- Error handling after core implementation

### Parallel Opportunities

**Phase 1 (Setup)**:
- T001, T002, T003 (directories) can run in parallel
- T004, T005, T006 (BUILD.bazel files) can run after directories, in parallel with each other

**Phase 2 (Foundational)**:
- T007-T015 (all data types) can run in parallel
- T016 (fixtures) can run in parallel with types
- T017-T020 (cache + API basics) can run after types complete

**Phase 3 (US1)**:
- T021-T024 (all tests) can run in parallel
- T025-T029 (API/Parser functions) can run in parallel after T020
- T030-T034 (CLI components) sequential after API/Parser

**Phase 4 (US2)**:
- T036-T037 (tests) can run in parallel
- T038-T041 (API/Parser/Display) can run in parallel after tests

**Phase 5 (US3)**:
- T045-T046 (tests) can run in parallel
- T047-T049 (API/Parser/Cache) can run in parallel

**Phase 6 (US4 - Deep Module Inspection)**:
- T054-T055 (tests) can run in parallel
- T056-T061 (API/Parser/Display components) have some dependencies but T057, T058, T060, T061 can partially parallel

**Phase 7 (US5 - Module Search)**:
- T066-T067 (tests) can run in parallel
- T068-T071 (search components) can run in parallel

**Phase 8 (Polish)**:
- T074-T079 (tests and flags) can run in parallel
- T083-T084 (env vars) can run in parallel
- T085-T086 (formatting) can run in parallel

**Cross-Story Parallelization**:
- Once Foundational complete, US1 and US2 can proceed simultaneously (different features)
- US3, US4, US5 can proceed simultaneously (different features)

---

## Parallel Example: Foundational Phase

```bash
# Launch all data type implementations together:
Task T007: "Implement Package data type in HackageClient/Types.hs"
Task T008: "Implement Version data type in HackageClient/Types.hs"
Task T009: "Implement Module data type in HackageClient/Types.hs"
Task T010: "Implement Function data type in HackageClient/Types.hs"
Task T011: "Implement Type data type in HackageClient/Types.hs"
Task T012: "Implement TypeClass data type in HackageClient/Types.hs"
Task T013: "Implement Dependency data type in HackageClient/Types.hs"
Task T014: "Implement QueryResult data type in HackageClient/Types.hs"
Task T015: "Implement CacheEntry data type in HackageClient/Types.hs"
Task T016: "Create test fixtures in test/Fixtures/"
# All can be worked on simultaneously in ~4 hours
```

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task T021: "QuickCheck property for Package JSON in ParserSpec.hs"
Task T022: "HUnit test for Package parsing in ParserSpec.hs"
Task T023: "HUnit test for cache write/read in CacheSpec.hs"
Task T024: "HUnit test for cache TTL in CacheSpec.hs"
# All tests can be written in parallel in ~2 hours

# Then launch API/Parser implementations:
Task T025: "Implement fetchPackageInfo in API.hs"
Task T026: "Implement parsePackageResponse in Parser.hs"
Task T029: "Implement similarity suggestions in Parser.hs"
# Can work in parallel in ~3 hours
```

---

## Implementation Strategy

### MVP First (User Story 1 + User Story 2)

1. Complete Phase 1: Setup (~1 hour)
2. Complete Phase 2: Foundational (~6 hours with parallelization)
3. Complete Phase 3: User Story 1 (~8 hours)
4. Complete Phase 4: User Story 2 (~4 hours)
5. **STOP and VALIDATE**: Test both stories independently
6. Deploy/demo working MVP (basic package query + version listing)

**MVP Deliverable**: Can query packages, show tree, list versions, use cache

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready (~7 hours)
2. Add User Story 1 + 2 → Test → Deploy/Demo (MVP!) (~12 hours, total ~19 hours)
3. Add User Story 3 → Test → Deploy (version-specific queries) (~6 hours, total ~25 hours)
4. Add User Story 4 with filters/comments → Test → Deploy (deep module inspection) (~14 hours, total ~39 hours)
5. Polish → Final release (~6 hours, total ~45 hours)

### Parallel Team Strategy

With 2 developers after Foundational phase:

1. Both complete Setup + Foundational together (~7 hours)
2. Split user stories:
   - Developer A: US1 (8h) → US3 (6h) = 14 hours
   - Developer B: US2 (4h) → US4 with filters/comments (14h) = 18 hours
3. Reconvene for Polish together (~6 hours)
4. **Total calendar time**: ~31 hours (vs. 50 hours sequential)

---

## Task Count Summary

- **Phase 1 (Setup)**: 6 tasks
- **Phase 2 (Foundational)**: 14 tasks (~6 hours with parallelization)
- **Phase 3 (US1)**: 15 tasks (~8 hours)
- **Phase 4 (US2)**: 9 tasks (~4 hours)
- **Phase 5 (US3)**: 9 tasks (~6 hours)
- **Phase 6 (US4 - Deep Module Inspection + Filters + Comments)**: 27 tasks (~14 hours)
  - Original US4: 12 tasks
  - Filter flags (FR-014): 9 tasks
  - --with-comments (FR-015): 6 tasks
- **Phase 7 (Polish)**: 11 tasks (~6 hours)

**Total: 91 tasks**

**Estimated Timeline**:
- Sequential: ~50 hours (~1.3 weeks)
- Parallel (2 devs): ~32 hours (~4 days)
- MVP only (US1+US2): ~19 hours (~2.5 days)

---

## Notes

- [P] tasks = different files, no dependencies, can parallelize
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- Tests use local fixtures per constitution (no live Hackage requests)
- All dependencies already in Stackage LTS-24.19 (no MODULE.bazel changes)
- Commit after each logical group of tasks
- Format with Ormolu before final commit
- Stop at any checkpoint to validate story independently
