# Implementation Plan: Tabletop RPG Ruleset Documentation & Query System

**Branch**: `001-dnd-ruleset-query` | **Date**: 2025-11-16 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-dnd-ruleset-query/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

A sophisticated multi-world RPG ruleset authoring and query system that enables creation of custom tabletop RPG rulesets with system inheritance, conditional/calculated rules, access control, and cross-world content portability. The system focuses on three core rule categories (Character Creation, World Building, Interactions) with file-based markdown + YAML frontmatter storage, query capabilities, validation workflows, and support for multiple game worlds sharing or extending base rulesets.

**Key Clarifications from Session 2025-11-16**:
- **Rule ID Format**: `^[A-Z]{2,6}-\d{3}$` (e.g., CHAR-001, CMBT-042, CHRGEN-100)
- **Tags**: Remain lowercase (e.g., mechanics, combat, attack)
- **Scalability**: Max 1000 rules/system, 10 systems, 5 concurrent users
- **Duplicate IDs**: Blocked with validation error
- **Prefix Conventions**: Suggested (CHAR, WRLD, INTR) but not enforced
- **Access Control**: CLI `--role` flag (gm/player), trust-based

**Technical Approach**: Haskell-based system leveraging Bazel build infrastructure, file-based markdown storage with YAML parsing, git for version control, and a modular architecture supporting complex rule relationships, formulas, and multi-world scenarios.

## Technical Context

**Language/Version**: Haskell GHC 9.10.1 (Stackage LTS-24.19)

**Primary Dependencies**:
- `yaml` (YAML frontmatter parsing with Aeson integration)
- `cmark` (Markdown parsing, may migrate to `commonmark-hs` for table extensions)
- `aeson` (JSON for export/import, YAML integration)
- `containers` (Map/Set for indexing - O(log n) lookups)
- `text` (text processing)
- `directory`, `filepath` (file system traversal)
- `optparse-applicative` (CLI interface - already in project)
- Future Phase 2c: `megaparsec` (formula/condition DSL parsing with AST validation)

**Storage**: File-based (Markdown + YAML frontmatter), directory hierarchy mirroring category structure, git for version history

**Testing**: Hspec or Tasty for unit tests, QuickCheck for property-based testing (rule validation, inheritance resolution)

**Target Platform**: Linux/macOS/Windows (CLI application)

**Project Type**: Single Haskell application with library components

**Performance Goals**:
- Query results < 2 seconds for typical searches (SC-001)
- Rule validation workflow < 2 minutes (SC-009)
- Support 10,000 total rules across multiple systems at max scale

**Constraints**:
- System-agnostic (no hardcoded RPG system assumptions)
- Version control friendly (human-readable diffs)
- No external database required
- File-based for simplicity and portability
- Max 1000 rules per system, 10 systems, 5 concurrent users

**Scale/Scope**:
- Initial: 1-2 core rulebooks per system (hundreds of rules)
- Growth: Multiple systems with variants, up to 1000 rules per system
- MVP: Single system with basic query/validation features
- Full: Multi-world with inheritance, conditionals, formulas, access control

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### ✅ I. Bazel-First Development
**Status**: PASS
**Evidence**: Project will create `haskell/app/rpg-ruleset-query` with appropriate BUILD.bazel, dependencies managed via MODULE.bazel + Stackage, no direct cabal/stack usage.

### ✅ II. Polyglot Integration
**Status**: PASS
**Evidence**: Pure Haskell application, no polyglot mixing required for this feature. Future integrations (if any) would go through Bazel.

### ✅ III. Library-Centric Architecture
**Status**: PASS
**Evidence**: Core functionality will be extracted to `haskell/libs/rpg-ruleset-core` for:
- YAML/Markdown parsing
- Rule data model (with new ID format: `^[A-Z]{2,6}-\d{3}$`)
- Query engine
- Validation logic (including duplicate ID blocking)
- System inheritance resolution

Application in `haskell/app/rpg-ruleset-query` will be thin CLI wrapper with `--role` flag for access control.

### ✅ IV. Test-Driven Quality
**Status**: PASS
**Evidence**: QuickCheck properties for:
- Rule inheritance resolution (variant overrides base correctly)
- Validation logic (conditional evaluation, formula validation, rule ID format validation)
- Export/import compatibility checks
- Rule ID uniqueness enforcement
Unit tests with fixture markdown files (not live file system tests in CI).

### ✅ V. Automated Formatting
**Status**: PASS
**Evidence**: All Haskell code formatted with Ormolu, BUILD.bazel with Buildifier, enforced in CI.

**Re-evaluation after Phase 1**: All constitutional requirements continue to pass with clarified specifications.

## Project Structure

### Documentation (this feature)

```text
specs/001-dnd-ruleset-query/
├── spec.md              # Feature specification (with Session 2025-11-16 clarifications)
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (technology decisions, patterns)
├── data-model.md        # Phase 1 output (entities, relationships, YAML schema)
├── quickstart.md        # Phase 1 output (getting started guide)
├── contracts/           # Phase 1 output (CLI interface, data schemas)
│   ├── cli-interface.md  # Command-line interface specification
│   ├── rule-schema.yaml  # YAML frontmatter schema definition
│   ├── query-api.md      # Query interface specification
│   └── validation-checklists.md  # Validation checklist definitions
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
haskell/libs/rpg-ruleset-core/
├── BUILD.bazel
└── src/
    ├── RpgRuleset/
    │   ├── Core/
    │   │   ├── Types.hs           # Core data types (Rule, System, World, etc.)
    │   │   ├── Rule.hs             # Rule entity with ID format validation
    │   │   ├── System.hs           # System entity with inheritance
    │   │   └── World.hs            # World entity
    │   ├── Parser/
    │   │   ├── Yaml.hs             # YAML frontmatter parsing
    │   │   ├── Markdown.hs         # Markdown content parsing
    │   │   ├── Formula.hs          # Formula syntax parsing (Phase 2c)
    │   │   └── Condition.hs        # Condition syntax parsing (Phase 2c)
    │   ├── Query/
    │   │   ├── Engine.hs           # Query engine (keyword + category matching)
    │   │   ├── Ranking.hs          # Relevance ranking algorithm
    │   │   └── Filter.hs           # Visibility filtering (via --role flag), system filtering
    │   ├── Validation/
    │   │   ├── Checklist.hs        # Validation checklist workflow
    │   │   ├── CategorySpecific.hs # Category-specific validation rules
    │   │   ├── Formulas.hs         # Formula validation (syntax, references) - Phase 2c
    │   │   ├── Conditions.hs       # Condition validation - Phase 2c
    │   │   └── RuleId.hs           # Rule ID format validation and duplicate checking
    │   ├── Inheritance/
    │   │   ├── Resolver.hs         # System inheritance resolution - Phase 2b
    │   │   └── CrossRef.hs         # Cross-system reference validation - Phase 2b
    │   ├── Export/
    │   │   └── Import.hs           # Export/import with validation - Phase 2d
    │   └── FileSystem/
    │       ├── Loader.hs           # File system traversal and loading
    │       └── Structure.hs        # Directory structure conventions
    └── test/
        ├── RpgRuleset/
        │   ├── Core/TypesSpec.hs
        │   ├── Query/EngineSpec.hs
        │   ├── Validation/ChecklistSpec.hs
        │   ├── Validation/RuleIdSpec.hs
        │   └── Inheritance/ResolverSpec.hs
        └── fixtures/               # Markdown + YAML test fixtures
            ├── base-system/
            ├── variant-system/
            └── test-rules/

haskell/app/rpg-ruleset-query/
├── BUILD.bazel
├── Main.hs                        # CLI entry point
└── src/
    ├── CLI/
    │   ├── Commands.hs             # Command definitions (query, validate, etc.)
    │   ├── Options.hs              # CLI option parsing (including --role flag)
    │   └── Output.hs               # Output formatting (markdown, tables)
    └── App.hs                      # Application orchestration

tests/rpg-ruleset-query/           # Integration tests
├── BUILD.bazel
└── integration/
    ├── QueryWorkflowSpec.hs
    ├── ValidationWorkflowSpec.hs
    ├── RuleIdValidationSpec.hs
    └── InheritanceSpec.hs
```

**Structure Decision**: Single Haskell application with library-centric architecture. Core logic in shared library (`rpg-ruleset-core`) for testability and reuse. CLI application is thin wrapper. This follows constitutional Library-Centric Architecture principle and enables future expansion (e.g., web interface, LSP server) by reusing core library.

## Complexity Tracking

> **No constitutional violations requiring justification.**

This feature aligns with all constitutional principles:
- Bazel-first development
- Library-centric with clear separation
- Test-driven with QuickCheck properties
- Standard Haskell project structure
- Automated formatting

**Complexity justified by requirements**:
- Multi-world support: Required by spec (FR-021 through FR-024)
- Conditional/formula support: Required by spec (FR-026, FR-027)
- Access control: Required by spec (FR-028) - implemented as simple CLI flag
- System inheritance: Required by spec (FR-021)
- Rule ID format validation: Required by clarifications (Session 2025-11-16)
- Scalability targets: Required by clarifications (1000 rules/system, 10 systems, 5 concurrent users)

All complexity is feature-driven, not architectural complexity for its own sake.

## Implementation Phases

### Phase 0: Research & Technology Validation
**Goal**: Resolve technology choices and validate approaches

**Status**: ✅ COMPLETE (see research.md)

**Research Topics**:
1. ✅ YAML frontmatter parsing libraries → `yaml` (Data.Yaml)
2. ✅ Markdown parsing → `cmark` (MVP) with migration path to `commonmark-hs`
3. ✅ Formula/condition DSL design → Two-phase (text storage → megaparsec parsing)
4. ✅ File system traversal → `directory` + `filepath`
5. ✅ Query ranking algorithms → Hybrid scoring (keyword + category + tag)
6. ✅ Git integration → External to application (YAML changelog only)

**Output**: ✅ `research.md` with technology decisions and rationale

### Phase 1: Core Data Model & Contracts
**Goal**: Define complete data model and interfaces

**Status**: ✅ COMPLETE

**Deliverables**:
1. ✅ **data-model.md**: Complete entity definitions
   - Rule (with rule ID format: `^[A-Z]{2,6}-\d{3}$`, all 17+ clarification features)
   - System (with inheritance metadata)
   - World
   - ValidationChecklist
   - ContentPackage (export/import) - Phase 2d
   - CrossReference - Phase 2b
   - Query

2. ✅ **contracts/**: Interface specifications
   - `rule-schema.yaml`: YAML frontmatter schema with rule ID validation
   - `cli-interface.md`: CLI command specifications (including --role flag)
   - `query-api.md`: Query interface contracts
   - `validation-checklists.md`: Category-specific validation definitions

3. ✅ **quickstart.md**: Getting started guide with examples using new rule ID format

**Output**: ✅ All Phase 1 deliverables complete

### Phase 2: Phased Implementation Planning
**Goal**: Break down into manageable implementation tasks

**Recommended MVP Scope** (Phase 2a):
- Single system support (no inheritance yet)
- Basic YAML + Markdown parsing
- **Rule ID validation** (format: `^[A-Z]{2,6}-\d{3}$`)
- **Duplicate ID blocking** (validation error)
- **Prefix convention warnings** (suggested: CHAR, WRLD, INTR)
- Simple query (keyword + category)
- Basic validation (duplicate IDs, tag similarity, rule ID format)
- File system loading
- Version tracking (simple changelog)
- Public visibility only (no GM-only yet)
- **Scalability**: Test with 1000 rules per system

**Phase 2b - Multi-World**:
- System inheritance
- Cross-system references
- Variant resolution
- **Scalability**: Test with 10 systems

**Phase 2c - Advanced Rules**:
- Conditional rules
- Formula parsing and validation (megaparsec)
- **GM-only visibility filtering** (via `--role` flag)
- **Concurrent user testing** (5 users)

**Phase 2d - Content Portability**:
- Export/import
- Validation warnings
- **Full scalability testing** (10,000 total rules)

## Next Steps

1. ✅ Execute Phase 0: Research (generated `research.md`)
2. ✅ Execute Phase 1: Design (generated `data-model.md`, `contracts/`, `quickstart.md`)
3. ✅ Update agent context with technology decisions (CLAUDE.md updated)
4. ⏭️ Execute `/speckit.tasks` to generate detailed implementation tasks

**Estimated Complexity**: HIGH
- 17+ clarifications to implement (including 5 from Session 2025-11-16)
- Multi-world architecture
- Conditional/formula DSL
- System inheritance resolution
- Rule ID format validation and duplicate blocking
- Scalability testing (1000 rules/system, 10 systems, 5 concurrent users)
- Phased delivery recommended (MVP → Full)

## Key Implementation Notes

### Rule ID Validation (New from Clarifications)

**Format**: `^[A-Z]{2,6}-\d{3}$`

**Implementation Requirements**:
1. Validation function in `RpgRuleset/Validation/RuleId.hs`:
   ```haskell
   validateRuleIdFormat :: Text -> Either ValidationError ()
   validateRuleIdFormat rid =
     if T.length prefix >= 2 && T.length prefix <= 6
        && T.all isUpper prefix
        && T.length num == 3
        && T.all isDigit num
     then Right ()
     else Left (InvalidRuleIdFormat rid)
     where
       (prefix, rest) = T.breakOn "-" rid
       num = T.drop 1 rest
   ```

2. Duplicate ID checking:
   ```haskell
   checkDuplicateIds :: [Rule] -> [ValidationError]
   checkDuplicateIds rules =
     let ids = map ruleId rules
         duplicates = ids \\ nub ids
     in map DuplicateRuleId duplicates
   ```

3. Prefix convention warnings:
   ```haskell
   suggestedPrefixes :: Map Category [Text]
   suggestedPrefixes = Map.fromList
     [ ("character-creation", ["CHAR", "CHRGEN", "CLASS", "ANCS"])
     , ("world-building", ["WRLD", "GEOG", "FACT", "MAGK"])
     , ("interactions", ["INTR", "SOCL", "SKILL"])
     ]

   warnNonConventionalPrefix :: Category -> RuleId -> Maybe Warning
   ```

### Access Control (New from Clarifications)

**CLI Flag Implementation**:
```haskell
-- CLI/Options.hs
data UserRole = Player | GM deriving (Show, Eq)

data QueryOptions = QueryOptions
  { qoKeywords :: [Text]
  , qoRole :: UserRole  -- Default: Player
  , ...
  }

roleOption :: Parser UserRole
roleOption = option auto
  ( long "role"
  <> metavar "ROLE"
  <> value Player
  <> help "User role (player|gm) - defaults to player"
  )
```

### Scalability Testing Requirements

**Performance Targets**:
- Query < 2 seconds at max scale (10,000 rules)
- Validation < 2 minutes per rule
- Concurrent user handling (5 users)

**Test Scenarios**:
1. Load 1000 rules in single system
2. Load 10 systems with 100 rules each
3. Query across all 10,000 rules
4. Concurrent validation by 5 users
5. Import/export with 1000-rule packages

## References

- [Spec](./spec.md) - User scenarios, requirements, and clarifications
- [Research](./research.md) - Technology decisions and validation
- [Data Model](./data-model.md) - Entity definitions with rule ID format
- [Contracts](./contracts/) - Interface specifications
- [Quickstart](./quickstart.md) - Getting started guide

**Planning complete. Ready for task generation via `/speckit.tasks`.**
