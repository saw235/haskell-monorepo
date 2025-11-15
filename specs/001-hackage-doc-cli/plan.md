# Implementation Plan: Hackage Documentation CLI Query Tool

**Branch**: `001-hackage-doc-cli` | **Date**: 2025-11-15 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/home/xsaw/haskell-monorepo/specs/001-hackage-doc-cli/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

A command-line tool for querying Hackage package documentation without leaving the terminal. The tool provides hierarchical tree views of packages showing modules, functions, types, and classes with their signatures. It supports version listing, version-specific queries, deep module inspection with source code, output filtering by entity type (--filter-functions, --filter-types, --filter-classes), optional Haddock documentation display (--with-comments), and local caching for performance. Users can query packages, list all available versions, view complete package trees with type signatures, inspect individual modules with full documentation and implementation code, and control output verbosity through filtering and comment flags.

## Technical Context

**Language/Version**: Haskell with GHC 9.10.1 (project uses Stackage LTS-24.19)
**Primary Dependencies**: http-conduit (2.3.9.1), aeson (2.2.3.0), optparse-applicative (already in project), containers (core), directory (core), text (core), scalpel (for HTML parsing)
**Storage**: File-based JSON cache in ~/.cache/hackage-cli/ with TTL-based invalidation (24h metadata, 7d versions, 30d immutable)
**Testing**: Tasty test framework with QuickCheck for property-based tests, HUnit for unit tests with local HTML/JSON fixtures
**Target Platform**: Linux, macOS, Windows (terminal-based CLI application)
**Project Type**: Single project (CLI application with library components)
**Performance Goals**: <2 seconds for single package queries (95%), output filtering adds negligible overhead (<50ms)
**Constraints**: Terminal UTF-8 support required, network connectivity required for Hackage access, 24-hour cache validity
**Scale/Scope**: Single CLI binary, potentially thousands of cached package metadata entries, handle packages with hundreds of modules

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### I. Bazel-First Development
- ✅ **COMPLIANT**: Will create BUILD.bazel with haskell_binary rule for CLI executable
- ✅ **COMPLIANT**: All dependencies will be declared through MODULE.bazel and Stackage
- ✅ **ACTION**: Need to add HTTP client, JSON/HTML parsing, and tree display libraries to MODULE.bazel

### II. Polyglot Integration
- ✅ **COMPLIANT**: Pure Haskell project, no cross-language integration required
- ✅ **COMPLIANT**: Will use Stackage LTS-24.19 ecosystem consistently

### III. Library-Centric Architecture
- ✅ **COMPLIANT**: Will create library in `haskell/libs/hackage-client/` for reusable Hackage API client
- ✅ **COMPLIANT**: CLI application in `haskell/app/hackage-doc-cli/` will consume the library
- ✅ **ACTION**: Library will expose public API for querying Hackage, parsing documentation, caching, filtering, and conditional display

### IV. Test-Driven Quality
- ✅ **COMPLIANT**: Will write QuickCheck properties for parsing, caching, tree display logic, filtering logic
- ✅ **COMPLIANT**: Unit tests with local HTML fixtures for Hackage response parsing (per constitution requirement)
- ✅ **COMPLIANT**: No live Hackage requests in tests
- ✅ **ACTION**: Create test fixtures from real Hackage responses including modules with varied exports for filter testing

### V. Automated Formatting
- ✅ **COMPLIANT**: Will use Ormolu for Haskell code formatting
- ✅ **COMPLIANT**: Will use Buildifier for BUILD.bazel files
- ✅ **COMPLIANT**: Formatting enforced in CI before merging

**Constitution Compliance**: PASS - No violations. All principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/001-hackage-doc-cli/
├── spec.md              # Feature specification (complete)
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
haskell/libs/hackage-client/
├── BUILD.bazel
├── HackageClient.hs           # Main module - public API
├── HackageClient/
│   ├── Types.hs               # Data types (Package, Module, Function, FilterOptions, DisplayOptions, etc.)
│   ├── API.hs                 # Hackage HTTP API interactions
│   ├── Parser.hs              # Parse Hackage HTML/JSON responses
│   ├── Cache.hs               # Local caching logic
│   ├── TreeDisplay.hs         # Tree formatting and display with filtering and comment toggling
│   └── Filter.hs              # Output filtering logic (functions/types/classes)

haskell/app/hackage-doc-cli/
├── BUILD.bazel
├── Main.hs                    # CLI entry point, argument parsing
└── CLI/
    ├── Options.hs             # Command-line option definitions (--filter-*, --with-comments, etc.)
    └── Commands.hs            # Command handlers (query, list-versions, etc.)

haskell/app/hackage-doc-cli/test/
├── BUILD.bazel
├── Fixtures/                  # Local HTML/JSON fixtures from Hackage
│   ├── aeson-package.json
│   ├── aeson-versions.json
│   ├── aeson-module.html     # Module with mixed exports for filter testing
│   └── base-prelude.html     # Large module for testing filters
├── ParserSpec.hs              # Parser tests with fixtures
├── CacheSpec.hs               # Cache behavior tests
├── FilterSpec.hs              # Filter logic tests (new)
└── TreeDisplaySpec.hs         # Tree formatting tests including --with-comments
```

**Structure Decision**: Single project structure chosen as this is a standalone CLI tool with embedded library. The library (`hackage-client`) is separated to enable potential reuse by other tools in the future, following Library-Centric Architecture principle.

## CLI Options Design

The tool supports the following command-line flags:

### Query Options
- `<package-name>`: Required positional argument for package to query
- `--version VERSION`: Query specific version (default: latest stable)
- `--list-versions`: List all available versions instead of showing package tree
- `--module MODULE`: Query specific module within package (e.g., `Data.Aeson`)

### Output Filtering Options (FR-014)
- `--filter-functions`: Show only exported functions (exclude types and classes)
- `--filter-types`: Show only exported types (exclude functions and classes)
- `--filter-classes`: Show only exported type classes (exclude functions and types)
- **Multiple filters can be combined**: `--filter-functions --filter-types` shows functions and types but not classes

### Display Options (FR-015)
- `--with-comments`: Include Haddock documentation text alongside type signatures
- **Default behavior** (no flag): Show only names and type signatures for concise output

### Examples
```bash
# Basic package query (concise output, all entity types)
hdoc aeson

# Show only functions with their Haddock comments
hdoc aeson --module Data.Aeson --filter-functions --with-comments

# Show types and classes only, signatures only (no comments)
hdoc aeson --module Data.Aeson --filter-types --filter-classes

# Query specific version with all details
hdoc aeson --version 2.0.0.0 --module Data.Aeson --with-comments
```

## Data Flow Architecture

### Query Flow with Filtering and Comments

```
User Input (CLI flags)
    ↓
Options.hs parses flags into:
  - PackageName
  - Maybe Version
  - Maybe ModuleName
  - FilterOptions (functions: Bool, types: Bool, classes: Bool)
  - DisplayOptions (withComments: Bool)
    ↓
Commands.hs routes to appropriate handler with options
    ↓
Cache.hs checks for cached data (check TTL)
    ↓ (cache miss or expired)
API.hs fetches from Hackage
    ↓
Parser.hs extracts entities (Functions, Types, TypeClasses)
    ↓
Filter.hs applies FilterOptions to entity lists
    ↓
TreeDisplay.hs formats output:
  - Conditional Haddock display based on DisplayOptions.withComments
  - Only includes filtered entity categories
    ↓
Output to terminal
```

### Filter Logic Design

**Filter.hs** will implement:

```haskell
data FilterOptions = FilterOptions
  { filterFunctions :: Bool  -- Show functions?
  , filterTypes :: Bool      -- Show types?
  , filterClasses :: Bool    -- Show classes?
  } deriving (Show, Eq)

-- Default: show all
defaultFilterOptions :: FilterOptions
defaultFilterOptions = FilterOptions True True True

-- Apply filters to module exports
applyFilters :: FilterOptions -> Module -> Module
applyFilters opts mod =
  mod { moduleExportedFunctions = if filterFunctions opts
                                   then moduleExportedFunctions mod
                                   else []
      , moduleExportedTypes = if filterTypes opts
                               then moduleExportedTypes mod
                               else []
      , moduleExportedClasses = if filterClasses opts
                                 then moduleExportedClasses mod
                                 else []
      }
```

### Display Options Design

**Types.hs** additions:

```haskell
data DisplayOptions = DisplayOptions
  { withComments :: Bool  -- Include Haddock documentation?
  } deriving (Show, Eq)

defaultDisplayOptions :: DisplayOptions
defaultDisplayOptions = DisplayOptions False  -- Concise by default
```

**TreeDisplay.hs** will conditionally include documentation:

```haskell
displayFunction :: DisplayOptions -> Function -> String
displayFunction opts func =
  functionName func <> " :: " <> functionSignature func
  <> if withComments opts
      then "\n  -- " <> functionDoc func
      else ""
```

## Complexity Tracking

> **No violations requiring justification**

All constitutional principles are satisfied without exceptions. The addition of filtering and comment display flags aligns with the existing architecture and adds no new complexity violations.

---

## Phase 0: Research Complete

**Status**: ✅ Complete (original research.md remains valid)

The original research covered:
- Hackage API structure
- HTML parsing with scalpel
- Caching strategies
- Tree display patterns

**New Requirements Analysis**:
- **Filter flags**: Standard CLI pattern, no new research needed. Implementation is straightforward conditional list filtering.
- **--with-comments flag**: Binary toggle for display verbosity, no new research needed. Haddock documentation is already part of parsed entities.

No additional research required. The existing research.md covers all technical decisions.

---

## Phase 1: Design & Contracts

### Updates Required

1. **data-model.md**:
   - Add `FilterOptions` data type
   - Add `DisplayOptions` data type
   - Ensure `Function`, `Type`, `TypeClass` entities include `documentation :: Text` field for Haddock comments

2. **quickstart.md**:
   - Update examples to show filter flag usage
   - Update examples to show --with-comments usage
   - Add "Common Workflows" section with filtering scenarios

3. **contracts/**:
   - CLI contract: Document all flags including --filter-* and --with-comments
   - TreeDisplay contract: Specify filtering and comment display behavior

---

## Implementation Notes

### Performance Considerations
- Filtering is applied post-parse, pre-display (minimal overhead)
- `--with-comments` may increase output size 2-5x but no computation overhead
- Cache stores full entity data; filtering applied on retrieval for flexibility

### Testing Strategy
- **Filter tests**: Unit tests with fixtures containing mixed entity types, verify correct filtering combinations
- **Comment display tests**: Verify output with/without --with-comments flag, check formatting
- **Integration tests**: Test all flag combinations with real fixtures

### Migration Impact
- **Breaking changes**: None. All new flags are optional with sensible defaults.
- **Backward compatibility**: Existing usage (`hdoc package-name`) continues to work identically.

---

**Plan Version**: 1.1 (Updated 2025-11-15 to include FR-014 and FR-015)
**Original Plan**: 1.0 (Created 2025-11-15)
