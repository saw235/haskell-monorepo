# Implementation Plan: Haskell Rich Terminal Library

**Branch**: `003-haskell-rich-library` | **Date**: 2025-11-15 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/003-haskell-rich-library/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

This plan extends the existing Haskell Rich terminal library with three major enhancements: (1) multi-line table cell support with automatic row height expansion, (2) terminal width detection with responsive rendering to prevent overflow, and (3) terminal capability detection with graceful color degradation. The library already provides core functionality for styled text, tables, panels, trees, and progress bars. This plan focuses on adding the platform-aware features requested by the user while maintaining the library's minimal dependency philosophy and composable API design.

## Technical Context

**Language/Version**: Haskell GHC 9.10.1 (Stackage LTS-24.19)
**Primary Dependencies**:
  - Existing: base, text
  - New: terminal-size (0.3.4) - Terminal width/height detection via ioctl/Console API
  - New: wcwidth (0.0.2) - Unicode display width calculation
  - Already available: ansi-terminal (1.1.3 in LTS-24.19) - Color capability detection
**Storage**: N/A (pure rendering library, no persistence)
**Testing**: Bazel test framework with QuickCheck for property-based testing
**Target Platform**: Cross-platform (Linux, macOS, Windows) terminal emulators with ANSI support
**Project Type**: Haskell library (haskell/libs/rich)
**Performance Goals**: Render tables/panels/trees in <50ms for typical CLI use (100 rows, 10 columns), support terminals up to 500 columns wide
**Constraints**:
  - Must maintain backward compatibility with existing API
  - Zero runtime overhead when terminal detection features not used
  - Maximum 4 additional dependencies (maintain minimal footprint)
  - Pure functional rendering where possible (IO only for terminal queries)
**Scale/Scope**:
  - Library enhancement (not new library)
  - ~7 new modules/functions for terminal detection
  - Support multi-line cells up to 50 lines per cell
  - Handle terminal widths from 40 to 500 columns

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Principle I: Bazel-First Development ✅ PASS
- All new dependencies will be added through MODULE.bazel with Stackage pinning
- BUILD.bazel exists at haskell/libs/rich with haskell_library rule
- Will update BUILD.bazel deps list after dependency research
- No direct cabal/stack usage

### Principle II: Polyglot Integration ✅ PASS
- This is a pure Haskell library enhancement
- Maintains Haskell-only scope, no ecosystem mixing
- New dependencies stay within Haskell/Stackage ecosystem

### Principle III: Library-Centric Architecture ✅ PASS
- Enhancing existing library in haskell/libs/rich
- Library is already public and independently testable
- Clear purpose: terminal output formatting with platform awareness
- Module structure follows existing Rich.* convention

### Principle IV: Test-Driven Quality ✅ PASS
- Will add QuickCheck properties for multi-line cell rendering
- Unit tests for terminal detection with mocked IO
- Tests for capability degradation paths
- All tests must pass before merging

### Principle V: Automated Formatting ✅ PASS
- All Haskell code will be formatted with Ormolu
- BUILD.bazel changes formatted with Buildifier
- CI enforcement already in place

**Gate Result**: ✅ ALL CHECKS PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
haskell/libs/rich/
├── src/
│   ├── Rich.hs                      # Main export module (existing)
│   ├── Rich/
│   │   ├── Style.hs                 # ANSI styling (existing)
│   │   ├── Console.hs               # Output functions (existing)
│   │   ├── Table.hs                 # Table rendering (existing - WILL MODIFY)
│   │   ├── Panel.hs                 # Panel rendering (existing - WILL MODIFY)
│   │   ├── Tree.hs                  # Tree rendering (existing)
│   │   ├── Progress.hs              # Progress bars (existing)
│   │   ├── Terminal.hs              # NEW: Terminal detection module
│   │   ├── Terminal/
│   │   │   ├── Size.hs              # NEW: Width/height detection
│   │   │   ├── Capability.hs        # NEW: Color capability detection
│   │   │   └── Width.hs             # NEW: Unicode width calculation
│   │   └── Internal/
│   │       └── MultiLine.hs         # NEW: Multi-line cell rendering utilities
├── test/
│   ├── Rich/
│   │   ├── StyleSpec.hs             # Existing tests
│   │   ├── TableSpec.hs             # WILL EXTEND: multi-line cell tests
│   │   ├── PanelSpec.hs             # WILL EXTEND: responsive rendering tests
│   │   ├── Terminal/
│   │   │   ├── SizeSpec.hs          # NEW: Terminal size tests
│   │   │   ├── CapabilitySpec.hs    # NEW: Capability detection tests
│   │   │   └── WidthSpec.hs         # NEW: Unicode width tests
│   │   └── MultiLineSpec.hs         # NEW: Multi-line rendering properties
└── BUILD.bazel                       # WILL UPDATE: add new dependencies

MODULE.bazel                          # WILL UPDATE: add terminal-size, unicode-width packages
non_module_deps.bzl                   # MAY UPDATE: if complex dependency configuration needed
```

**Structure Decision**: Library enhancement following existing Rich module structure. New functionality organized under Rich.Terminal.* namespace for platform detection features, with Rich.Internal.MultiLine for shared multi-line rendering logic. Tests mirror source structure with Spec suffix convention.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

N/A - No constitutional violations. All principles pass.

---

## Post-Design Constitution Re-Check

*Performed after Phase 1 design completion*

### Principle I: Bazel-First Development ✅ PASS
- Dependencies identified: terminal-size, wcwidth, ansi-terminal (already in LTS)
- Will add to MODULE.bazel _SIMPLE_PACKAGES list
- BUILD.bazel update planned for haskell/libs/rich
- No complex dependency configuration needed

### Principle II: Polyglot Integration ✅ PASS
- All dependencies are Haskell packages from Stackage
- No ecosystem mixing
- Maintains consistency with existing patterns

### Principle III: Library-Centric Architecture ✅ PASS
- Module structure follows Rich.* convention
- Clear separation: Rich.Terminal.* for new features
- Rich.Internal.* for implementation details
- Public API well-defined in contracts/api-contracts.md

### Principle IV: Test-Driven Quality ✅ PASS
- Test structure defined in data-model.md section 10
- QuickCheck properties specified
- Unit tests with mocked IO for terminal detection
- Integration tests for responsive rendering

### Principle V: Automated Formatting ✅ PASS
- All code will be formatted with Ormolu
- Bazel changes with Buildifier
- No changes needed to CI configuration

**Final Gate Result**: ✅ ALL CHECKS PASS - Ready for Phase 2 (task generation)

---

## Phase Completion Status

- ✅ **Phase 0: Research** - Completed (research.md)
  - Dependency selection: terminal-size, wcwidth, ansi-terminal
  - Cross-platform compatibility verified
  - Performance expectations documented

- ✅ **Phase 1: Design** - Completed
  - data-model.md: Entity definitions and data flow
  - contracts/api-contracts.md: Module contracts and API specifications
  - quickstart.md: Developer usage guide with examples
  - Agent context updated (CLAUDE.md)

- ⏹️ **Phase 2: Task Generation** - Not started (use `/speckit.tasks` command)

---

## Next Steps

To proceed with implementation:

```bash
/speckit.tasks
```

This will generate `tasks.md` with ordered, actionable implementation tasks based on:
- Functional requirements from spec.md
- Technical design from data-model.md
- API contracts from contracts/api-contracts.md
- Dependency choices from research.md
