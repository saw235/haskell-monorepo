# Tasks: Tabletop RPG Ruleset Documentation & Query System

**Input**: Design documents from `/specs/001-dnd-ruleset-query/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Not explicitly requested in spec - focusing on implementation tasks with QuickCheck properties as specified in plan.md

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story. MVP focuses on User Story 1 (Query Character Creation Rules), User Story 5 (Validate New Rules), and User Story 6 (Create and Add New Rules via CLI) as all three are marked P1.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3...)
- Include exact file paths in descriptions

## Path Conventions

Per plan.md:
- **Library**: `haskell/libs/rpg-ruleset-core/src/RpgRuleset/`
- **Application**: `haskell/app/rpg-ruleset-query/src/`
- **Tests**: `haskell/libs/rpg-ruleset-core/test/` and `tests/rpg-ruleset-query/integration/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and Bazel build configuration

- [X] T001 Create library directory structure at haskell/libs/rpg-ruleset-core/
- [X] T002 Create BUILD.bazel for rpg-ruleset-core library with haskell_library rule
- [X] T003 [P] Create application directory structure at haskell/app/rpg-ruleset-query/
- [X] T004 [P] Create BUILD.bazel for rpg-ruleset-query app with haskell_binary rule
- [X] T005 Add dependencies to MODULE.bazel: yaml, cmark, aeson, containers, text, directory, filepath, optparse-applicative
- [X] T006 Run bazel run @stackage-unpinned//:pin -- --upgrade-hackage to pin dependencies
- [X] T007 [P] Create test directory structures for unit and integration tests
- [X] T008 [P] Create fixture directory at haskell/libs/rpg-ruleset-core/test/fixtures/ with sample markdown files

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and infrastructure that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T009 Create core types in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Core/Types.hs (RuleId newtype with format `^[A-Z]{2,6}-\d{3}$`, Category, Tag, Visibility, Version, ChangelogEntry)
- [X] T010 [P] Implement Rule entity in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Core/Rule.hs with all metadata fields from data-model.md
- [X] T011 [P] Implement System entity in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Core/System.hs with SystemType (Base/Variant)
- [X] T012 [P] Implement World entity in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Core/World.hs
- [X] T013 Implement YAML parser in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Parser/Yaml.hs using Data.Yaml for frontmatter
- [X] T014 [P] Implement Markdown parser in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Parser/Markdown.hs using cmark
- [X] T015 Create rule ID validation module in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Validation/RuleId.hs with format validation function
- [X] T016 [P] Implement duplicate ID checking function in RuleId.hs with error blocking (FR-018b)
- [X] T017 [P] Implement prefix convention suggestions in RuleId.hs (CHAR, WRLD, INTR) with warnings only (FR-018c)
- [X] T018 Create file system loader in haskell/libs/rpg-ruleset-core/src/RpgRuleset/FileSystem/Loader.hs using directory and filepath
- [X] T019 [P] Implement directory structure conventions in haskell/libs/rpg-ruleset-core/src/RpgRuleset/FileSystem/Structure.hs
- [X] T020 Create query index structure in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Query/Index.hs with Map-based indexes (qiAllRules, qiByCategory, qiBySystem, qiByTag, qiPublicRules)
- [X] T021 Implement index building from loaded systems in Query/Index.hs
- [X] T022 [P] Create CLI options parser in haskell/app/rpg-ruleset-query/src/CLI/Options.hs with UserRole (Player/GM) and --role flag
- [X] T023 [P] Create CLI commands structure in haskell/app/rpg-ruleset-query/src/CLI/Commands.hs (query, validate, list, info)
- [X] T024 [P] Create CLI output formatter in haskell/app/rpg-ruleset-query/src/CLI/Output.hs for text/json/markdown formats

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Query Character Creation Rules (Priority: P1) 🎯 MVP

**Goal**: Enable users to quickly query character creation rules using keyword search with category filtering

**Independent Test**: Query "character attribute generation" and receive all valid methods with complete rules

### Implementation for User Story 1

- [X] T025 [P] [US1] Implement Query data type in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Query/Engine.hs (qKeywords, qFilterCategory, qFilterSystem, qFilterTags, qFilterVisibility with Player default)
- [X] T026 [P] [US1] Implement QueryResult and ScoredRule types in Query/Engine.hs
- [X] T027 [US1] Implement applyFilters function in Query/Engine.hs (category, system, tags, visibility filtering)
- [X] T028 [US1] Implement scoring algorithm in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Query/Ranking.hs (titleScore * 5.0, contentScore * 1.0, tagScore * 2.0, categoryScore * 10.0)
- [X] T029 [US1] Implement scoreRule function in Query/Ranking.hs with keyword matching
- [X] T030 [US1] Implement executeQuery function in Query/Engine.hs combining filtering, scoring, sorting, pagination
- [X] T031 [US1] Implement visibility filter in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Query/Filter.hs based on UserRole
- [X] T032 [US1] Implement query command handler in CLI/Commands.hs calling executeQuery with parsed options
- [X] T033 [US1] Implement query result formatting in CLI/Output.hs for text output (showing rule ID, title, category, tags, score)
- [X] T034 [US1] Add JSON output format support in CLI/Output.hs for query results
- [X] T035 [US1] Wire query command to Main.hs in haskell/app/rpg-ruleset-query/Main.hs
- [X] T036 [US1] Create fixture markdown files in test/fixtures/ for character creation rules (attributes, classes, ancestries) with rule IDs like CHAR-001, CLASS-001
- [ ] T037 [US1] Create QuickCheck properties in haskell/libs/rpg-ruleset-core/test/RpgRuleset/Query/EngineSpec.hs for query filtering correctness

**Checkpoint**: User Story 1 complete - can query character creation rules with keyword search and category filtering

---

## Phase 4: User Story 5 - Validate New Rules for Consistency (Priority: P1) 🎯 MVP

**Goal**: Enable creators to validate new rules for duplicates, format errors, and consistency

**Independent Test**: Add a rule with duplicate ID and receive validation error blocking save

### Implementation for User Story 5

- [ ] T038 [P] [US5] Create ValidationChecklist type in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Validation/Checklist.hs (checklistId, forCategory, items with severity)
- [ ] T039 [P] [US5] Create ValidationResult type in Validation/Checklist.hs (validatedRule, checklistUsed, results, overallStatus)
- [X] T040 [US5] Implement checkRuleIdFormat in Validation/RuleId.hs validating `^[A-Z]{2,6}-\d{3}$` pattern
- [X] T041 [US5] Implement checkDuplicateIds in Validation/RuleId.hs blocking with error (FR-018b)
- [X] T042 [US5] Implement checkPrefixConventions in Validation/RuleId.hs with warnings (FR-018c) for non-standard prefixes
- [ ] T043 [P] [US5] Implement tag similarity check in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Validation/CategorySpecific.hs using Levenshtein distance
- [ ] T044 [P] [US5] Implement related rules exist check in Validation/CategorySpecific.hs
- [ ] T045 [P] [US5] Implement category-specific checklist definitions in contracts/validation-checklists.md patterns (character-creation, world-building, interactions)
- [ ] T046 [US5] Create validation workflow executor in Validation/Checklist.hs combining all checks
- [X] T047 [US5] Implement validate command handler in CLI/Commands.hs loading file and running validation
- [X] T048 [US5] Implement validation result formatting in CLI/Output.hs showing passed/failed/warning checks with severity
- [X] T049 [US5] Add --strict flag support in CLI/Options.hs to treat warnings as errors
- [X] T050 [US5] Wire validate command to Main.hs
- [ ] T051 [US5] Create QuickCheck properties in haskell/libs/rpg-ruleset-core/test/RpgRuleset/Validation/RuleIdSpec.hs for format validation correctness
- [ ] T052 [US5] Create fixtures with invalid rule IDs (wrong format, duplicates) in test/fixtures/ for validation testing

**Checkpoint**: User Story 5 complete - can validate rules with format checking, duplicate detection, and category-specific checks

---

## Phase 5: User Story 6 - Create and Add New Rules via CLI (Priority: P1) 🎯 MVP

**Goal**: Enable creators to add rules via CLI with interactive and non-interactive modes

**Independent Test**: Run `rpg-ruleset-query add --system my-rpg --interactive` and follow guided prompts to create a Character Creation rule, then verify it appears in query results

### Implementation for User Story 6

- [ ] T053 [P] [US6] Create AddOptions data type in haskell/app/rpg-ruleset-query/src/CLI/Options.hs (system, category, id, title, visibility, tags, file, interactive, skipValidation)
- [ ] T054 [US6] Implement add command option parser in CLI/Options.hs with optparse-applicative
- [ ] T055 [P] [US6] Create AddResult data type in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Core/Types.hs (AddSuccess, AddFailure with error details)
- [ ] T056 [US6] Implement rule ID suggestion logic in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Validation/RuleId.hs (suggestNextId function based on existing IDs in category)
- [ ] T057 [US6] Implement non-interactive add handler in haskell/app/rpg-ruleset-query/src/CLI/Commands.hs (parseOptions, validate, createFile, index)
- [ ] T058 [US6] Implement interactive mode prompts in CLI/Commands.hs (category selection, ID suggestion, metadata entry, content input)
- [ ] T059 [US6] Implement file-based rule parsing in CLI/Commands.hs for --file option (parse YAML frontmatter, extract rule metadata)
- [ ] T060 [US6] Implement rule file creation in haskell/libs/rpg-ruleset-core/src/RpgRuleset/FileSystem/Writer.hs (write markdown file with YAML frontmatter)
- [ ] T061 [US6] Implement immediate indexing after add in FileSystem/Loader.hs (reload index with new rule)
- [ ] T062 [P] [US6] Implement batch rule addition support in FileSystem/Writer.hs for files with multiple rules
- [ ] T063 [US6] Integrate validation workflow into add command (run validation after add unless --skip-validation)
- [ ] T064 [US6] Wire add command to Main.hs in haskell/app/rpg-ruleset-query/Main.hs
- [ ] T065 [US6] Implement add result formatting in CLI/Output.hs (success message, warnings, error details)
- [ ] T066 [P] [US6] Create fixtures for add command testing in test/fixtures/add-test-inputs/
- [ ] T067 [US6] Create QuickCheck properties in haskell/libs/rpg-ruleset-core/test/RpgRuleset/Validation/RuleIdSuggestionSpec.hs for ID suggestion correctness

**Checkpoint**: User Story 6 complete - creators can add rules via CLI with validation

---

## Phase 6: User Story 2 - Query World Building Rules (Priority: P2)

**Goal**: Enable GMs to query world building rules for campaign design

**Independent Test**: Query "settlement size categories" and receive population ranges and characteristics

### Implementation for User Story 2

- [ ] T068 [US2] Create fixture markdown files in test/fixtures/ for world building rules (settlements, magic items, environments) with rule IDs like WRLD-001, GEOG-001
- [ ] T069 [US2] Verify query engine works for world-building category by testing against new fixtures
- [ ] T070 [US2] Add world-building category filter example to CLI help text

**Checkpoint**: User Story 2 complete - world building queries work using existing query infrastructure

---

## Phase 7: User Story 3 - Query Interaction & Social Rules (Priority: P2)

**Goal**: Enable players/GMs to query interaction rules during gameplay

**Independent Test**: Query "persuasion check rules" and receive skill check mechanics

### Implementation for User Story 3

- [ ] T071 [US3] Create fixture markdown files in test/fixtures/ for interaction rules (skill checks, social encounters, influence) with rule IDs like INTR-001, SOCL-001
- [ ] T072 [US3] Verify query engine works for interactions category by testing against new fixtures
- [ ] T073 [US3] Add interactions category filter example to CLI help text

**Checkpoint**: User Story 3 complete - interaction queries work using existing query infrastructure

---

## Phase 8: User Story 8 - GM-Only Content Access Control (Priority: P2)

**Goal**: Enable GMs to hide spoilers from players using visibility metadata

**Independent Test**: Create rule marked `visibility: gm-only`, query as player (hidden) vs GM (visible)

### Implementation for User Story 8

- [ ] T074 [P] [US8] Verify --role flag parsing in CLI/Options.hs defaults to Player
- [ ] T075 [US8] Test visibility filtering in Query/Filter.hs with Player role (only public rules)
- [ ] T076 [US8] Test visibility filtering with GM role (all rules including gm-only)
- [ ] T077 [US8] Create fixture markdown files with gm-only visibility rules in test/fixtures/
- [ ] T078 [US8] Add visibility filter examples to CLI help text and quickstart.md
- [ ] T079 [US8] Implement hidden reference indicator in CLI/Output.hs when player views rule referencing GM-only rule

**Checkpoint**: User Story 8 complete - GM-only content properly hidden from players via --role flag

---

## Phase 9: User Story 4 - Cross-Reference Related Rules (Priority: P3)

**Goal**: Enable users to find related rules and understand rule interactions

**Independent Test**: Query "multiclassing" and receive rules showing rule relationships

### Implementation for User Story 4

- [ ] T080 [P] [US4] Implement --show-related flag in CLI/Options.hs to display related rules
- [ ] T081 [US4] Enhance query result formatting in CLI/Output.hs to show related rule IDs
- [ ] T082 [US4] Implement lookupRule helper in Query/Index.hs to fetch related rules by ID
- [ ] T083 [US4] Add recursive related rules traversal option (depth-limited to avoid cycles)
- [ ] T084 [US4] Create fixture markdown files with complex rule relationships in test/fixtures/
- [ ] T085 [US4] Add related rules query examples to quickstart.md

**Checkpoint**: User Story 4 complete - can discover and navigate related rules

---

## Phase 10: User Story 7 - Manage Multi-World System Variants (Priority: P2)

**Goal**: Enable system inheritance where variants extend base systems

**Independent Test**: Create base system + variant, verify variant inherits base rules unless overridden

**Note**: This implements Phase 2b from plan.md (Multi-World architecture)

### Implementation for User Story 7

- [ ] T086 [P] [US7] Implement system inheritance resolver in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Inheritance/Resolver.hs
- [ ] T087 [US7] Implement rule override logic in Inheritance/Resolver.hs (variant rules override base with same ID)
- [ ] T088 [US7] Implement cross-system reference validation in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Inheritance/CrossRef.hs
- [ ] T089 [US7] Update file system loader in FileSystem/Loader.hs to detect system.yaml and resolve inheritance
- [ ] T090 [US7] Implement circular inheritance detection in Inheritance/Resolver.hs
- [ ] T091 [P] [US7] Create ContentPackage type in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Export/Import.hs (packageId, rules, dependencies, validationWarnings)
- [ ] T092 [P] [US7] Implement export command in CLI/Commands.hs to create ContentPackage JSON
- [ ] T093 [US7] Implement import command in CLI/Commands.hs with compatibility validation
- [ ] T094 [US7] Implement --resolve-conflicts option in CLI/Options.hs (fail/skip/rename)
- [ ] T095 [US7] Create fixture directories for base-system and variant-system in test/fixtures/
- [ ] T096 [US7] Create QuickCheck properties in haskell/libs/rpg-ruleset-core/test/RpgRuleset/Inheritance/ResolverSpec.hs for inheritance correctness
- [ ] T097 [US7] Add export/import examples to quickstart.md

**Checkpoint**: User Story 7 complete - multi-world variants with inheritance and content portability work

---

## Phase 11: Advanced Rules - Conditional & Formula Support (Phase 2c)

**Goal**: Add conditional rules and formula parsing for advanced rule mechanics

**Note**: This implements Phase 2c from plan.md

### Implementation

- [ ] T098 [P] Create Formula parser in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Parser/Formula.hs using megaparsec (FormulaAST: DiceRoll, Variable, BinOp)
- [ ] T099 [P] Create Condition parser in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Parser/Condition.hs using megaparsec (ConditionAST: CompareOp, LogicalOp)
- [ ] T100 Add megaparsec dependency to MODULE.bazel and re-pin
- [ ] T101 Implement formula validation in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Validation/Formulas.hs (syntax check, variable reference check)
- [ ] T102 [P] Implement condition validation in haskell/libs/rpg-ruleset-core/src/RpgRuleset/Validation/Conditions.hs
- [ ] T103 Integrate formula/condition validation into validation workflow in Validation/Checklist.hs
- [ ] T104 Add --show-formulas flag in CLI/Options.hs to display formula metadata
- [ ] T105 Update CLI/Output.hs to render formulas from metadata
- [ ] T106 Create fixtures with formulas and conditions in test/fixtures/
- [ ] T107 Create QuickCheck properties for formula parsing in test/RpgRuleset/Parser/FormulaSpec.hs

**Checkpoint**: Advanced rules complete - formulas and conditions parsed and validated

---

## Phase 12: Additional CLI Commands

**Goal**: Implement remaining CLI commands from contracts/cli-interface.md

### Implementation

- [ ] T108 [P] Implement list command handler in CLI/Commands.hs (list systems, categories, rules)
- [ ] T109 [P] Implement info command handler in CLI/Commands.hs to show detailed rule information
- [ ] T110 [P] Implement init command handler in CLI/Commands.hs to create new system structure
- [ ] T111 Implement list systems output formatting in CLI/Output.hs
- [ ] T112 [P] Implement list categories output with tree format in CLI/Output.hs
- [ ] T113 [P] Implement list rules output in CLI/Output.hs
- [ ] T114 Implement info command output with full rule details, changelog, related rules in CLI/Output.hs
- [ ] T115 Implement init command to create system.yaml, category directories, README.md
- [ ] T116 Add all new commands to Main.hs routing
- [ ] T117 Update CLI help text with all available commands

**Checkpoint**: All CLI commands implemented per contracts/cli-interface.md

---

## Phase 13: Polish & Performance

**Goal**: Optimize, test scalability, and prepare for production use

### Implementation

- [ ] T118 [P] Create scalability test suite in tests/rpg-ruleset-query/integration/ testing 1000 rules per system
- [ ] T119 [P] Test query performance with 10 systems (10,000 total rules) to verify < 2 second target
- [ ] T120 [P] Test validation performance to verify < 2 minute target per rule
- [ ] T121 [P] Test concurrent user scenario (5 users querying simultaneously)
- [ ] T122 Optimize query index building if performance tests fail
- [ ] T123 [P] Add comprehensive error messages for all validation failures
- [ ] T124 [P] Add logging throughout application for debugging
- [ ] T125 Format all Haskell code with Ormolu
- [ ] T126 [P] Format all BUILD.bazel files with Buildifier
- [ ] T127 Run bazel build //... to verify all targets compile
- [ ] T128 Run bazel test //... to verify all tests pass (including QuickCheck properties)
- [ ] T129 Update quickstart.md with complete examples using final CLI
- [ ] T130 [P] Create example ruleset in rulesets/systems/tutorial-rpg/ per quickstart.md
- [ ] T131 [P] Update CLAUDE.md with final implementation notes

**Checkpoint**: System polished, tested, and ready for use

---

## Implementation Strategy

### MVP (Minimum Viable Product)

**Scope**: Phase 1-5 (includes new User Story 6 - Add Rules via CLI)
- User Story 1 (Query Character Creation Rules) - P1
- User Story 5 (Validate New Rules) - P1
- User Story 6 (Create and Add New Rules via CLI) - P1 **NEW**

**Deliverable**: Working query, validation, and rule addition system for single system with rule ID format enforcement

**Timeline**: Shortest path to value - users can query, validate, AND add character creation rules

### Incremental Delivery

**Phase 2 MVP**: Add Phase 6-7 (World Building + Interaction queries)
**Phase 3 MVP**: Add Phase 8 (GM-only access control)
**Phase 4 MVP**: Add Phase 9-10 (Related rules + Multi-world variants)
**Phase 5 MVP**: Add Phase 11 (Formulas and conditions)
**Final Release**: Phase 12-13 (All CLI commands + polish)

---

## Dependencies

### User Story Completion Order

```
Phase 1-2 (Setup + Foundation) → MUST complete first
    ↓
Phase 3 (US1: Query) ──────────┐
Phase 4 (US5: Validate) ───────┼── MVP milestone (all P1)
Phase 5 (US6: Add Rules CLI) ──┘   **NEW**
    ↓
Phase 6 (US2: World Building) ──┐
Phase 7 (US3: Interactions)     ├── Can run in parallel
Phase 8 (US8: GM-only)          ┘
    ↓
Phase 9 (US4: Related rules) ← Independent
    ↓
Phase 10 (US7: Multi-world) ← Requires US1+US5+US6 foundation
    ↓
Phase 11 (Formulas/Conditions) ← Independent enhancement
    ↓
Phase 12 (CLI commands) ← Can start after US1
    ↓
Phase 13 (Polish) ← Final phase
```

### Parallel Execution Opportunities

**Within Foundation (Phase 2)**:
- T010-T012 (entities) can run in parallel
- T013-T014 (parsers) can run in parallel
- T015-T017 (validation) can run in parallel
- T022-T024 (CLI) can run in parallel

**Within User Story 1 (Phase 3)**:
- T025-T026 (types) in parallel
- T036-T037 (fixtures + tests) in parallel

**Within User Story 5 (Phase 4)**:
- T038-T039 (types) in parallel
- T043-T045 (category checks) in parallel
- T051-T052 (fixtures + tests) in parallel

**Within User Story 6 (Phase 5)** - NEW:
- T053, T055 (option types) in parallel
- T062, T066 (batch + fixtures) in parallel

**Across User Stories**:
- Phase 6-8 (US2, US3, US8) are independent - can implement in any order or in parallel
- Phase 11 (formulas) is independent from Phase 9-10

---

## Task Summary

**Total Tasks**: 131
**Setup**: 8 tasks (T001-T008)
**Foundation**: 16 tasks (T009-T024)
**User Story 1 (P1)**: 13 tasks (T025-T037)
**User Story 5 (P1)**: 15 tasks (T038-T052)
**User Story 6 (P1)**: 15 tasks (T053-T067) **NEW - Add Rules via CLI**
**User Story 2 (P2)**: 3 tasks (T068-T070)
**User Story 3 (P2)**: 3 tasks (T071-T073)
**User Story 8 (P2)**: 6 tasks (T074-T079)
**User Story 4 (P3)**: 6 tasks (T080-T085)
**User Story 7 (P2)**: 12 tasks (T086-T097)
**Advanced Rules**: 10 tasks (T098-T107)
**CLI Commands**: 10 tasks (T108-T117)
**Polish**: 14 tasks (T118-T131)

**MVP (Phase 1-5)**: 67 tasks (includes new User Story 6)
**Parallel Opportunities**: 52 tasks marked [P]

**Format Validation**: ✅ All tasks follow checklist format: `- [ ] [ID] [P?] [Story?] Description with file path`

---

## Notes

- All tasks include specific file paths per plan.md structure
- QuickCheck properties specified per constitutional Test-Driven Quality principle
- Scalability testing targets: 1000 rules/system, 10 systems, 5 concurrent users
- Rule ID format `^[A-Z]{2,6}-\d{3}$` enforced throughout
- Access control via CLI `--role` flag (trust-based, defaults to player)
- Test fixtures required for each user story to enable independent testing
