<!--
Sync Impact Report:
- Version change: 1.0.0 → 1.1.0 (MINOR - new principle added)
- Modified principles: None
- Added sections: VI. Requirements Traceability
- Removed sections: None
- Templates requiring updates:
  ✅ tasks-template.md - updated task format and examples to include FR tags
  ✅ spec-template.md - already has FR definitions (no changes needed)
  ✅ plan-template.md - reviewed, no updates needed (constitution check references updated constitution)
- Follow-up TODOs: None - all placeholders resolved
-->

# Haskell Monorepo Constitution

## Core Principles

### I. Bazel-First Development
All builds and dependency management MUST go through Bazel. Never use cabal/stack directly for Haskell packages or npm/yarn for NodeJS dependencies. All components require BUILD.bazel files with appropriate haskell_binary/haskell_library rules. Platform-specific configurations are maintained in .bazelrc for cross-platform compatibility.

**Rationale**: Unified build system ensures reproducible builds, consistent dependency management across polyglot environments, and eliminates "works on my machine" issues.

### II. Polyglot Integration
Multi-language ecosystem with unified dependency management across Haskell, TypeScript, and NodeJS. Each ecosystem maintains its own patterns (Stackage for Haskell, PNPM for NodeJS) but all coordinated through Bazel. Complex packages require configuration in non_module_deps.bzl.

**Rationale**: Leverages best-of-breed tools while maintaining architectural consistency and avoiding ecosystem mixing that leads to maintenance burden.

### III. Library-Centric Architecture
Shared libraries MUST be self-contained, independently testable, and documented. Each library requires clear purpose and public visibility declarations when needed by applications. Libraries live in `haskell/libs/` with standardized module structures.

**Rationale**: Promotes code reuse, testability, and maintainable boundaries between components while preventing organizational-only libraries that lack clear purpose.

### IV. Test-Driven Quality
Comprehensive testing with QuickCheck property-based testing for Haskell components. All tests MUST pass before merging. Web scraping requires unit tests with local HTML fixtures, not live requests. CI configuration available with limited parallelism.

**Rationale**: Ensures code quality, prevents regressions, and maintains system reliability through both unit and property-based testing approaches.

### V. Automated Formatting
Code formatting MUST be enforced across all languages: Ormolu for Haskell, Prettier for TypeScript, Buildifier for Bazel files. Formatting is automated in CI pipeline and MUST pass before merging. Use appropriate formatters for each ecosystem.

**Rationale**: Eliminates bikeshedding, ensures consistent code style across the polyglot codebase, and reduces cognitive load during code reviews.

### VI. Requirements Traceability
Every task in tasks.md MUST be tagged with the specific Functional Requirement(s) (FR-XXX) it implements. Every test task MUST reference the FR(s) it validates. This bidirectional traceability links requirements → tasks → tests → implementation, enabling impact analysis, coverage tracking, and requirement verification.

**Rationale**: Ensures complete requirement coverage, enables quick impact assessment when requirements change, facilitates test-to-requirement mapping for compliance and auditing, and prevents orphaned tasks or untested requirements. Clear traceability is essential for maintainability and understanding the purpose of every code change.

## Architecture Standards

All applications reside in `haskell/app/` as standalone executables with clear separation from shared libraries. Electron applications integrate with Haskell backends via well-defined interfaces (WebSocket, CLI). Docker environment available for web scraping with Chrome/Selenium pre-installed. External dependencies managed through Bazel's external directory structure.

## Development Workflow

Format code before committing (enforced in CI). Use Docker environment for web scraping development. Test Electron apps locally before committing. Follow existing patterns for new components. Respect the polyglot architecture and avoid inappropriate ecosystem mixing. Pin dependencies after any MODULE.bazel changes using the standardized pin command.

## Governance

This Constitution supersedes all other development practices. Amendments require documentation, approval, and migration plan. All PRs and reviews MUST verify compliance with these principles. Complexity MUST be justified against the architectural standards. CLAUDE.md provides runtime development guidance that complements these constitutional requirements.

**Version**: 1.1.0 | **Ratified**: 2025-11-14 | **Last Amended**: 2025-11-15
