# Feature Specification: Hackage Documentation CLI Query Tool

**Feature Branch**: `001-hackage-doc-cli`
**Created**: 2025-11-14
**Status**: Draft
**Input**: User description: "Let's create a cli tool to query hackage for documentation."

## Clarifications

### Session 2025-11-15

- Q: What is the primary interaction model for this tool? → A: Single command-line queries only (e.g., `hdoc langchain-hs` displays info and exits), with flags for options
- Q: How should hierarchical information be displayed in the tree output? → A: Always show the entire tree (Package → Modules → Functions/Types/Classes). Users can query specific modules for detailed information by specifying both package and module path with a flag.
- Q: When querying a specific module, what detailed information should be displayed? → A: Full details including source code (signatures, documentation, and actual implementation code)
- Q: For the tree view of a package, what level of detail should be shown for each exported item? → A: Names with type signatures (e.g., `parseChain :: Text -> Maybe Chain`)
- Q: When showing types in the tree, what information should be displayed? → A: Type definitions with constructors/methods (e.g., `data ChainConfig = ChainConfig { ... }`, show type class methods)
- Q: Should the tool support listing available versions and querying version-specific information? → A: Yes, support listing all available versions of a package, and use a --version flag to select specific versions when querying package/module information
- Q: Should users be able to filter module output to show only specific categories of exports (functions, types, classes)? → A: Add optional flags: --filter-functions, --filter-types, --filter-classes to show only specific categories (can combine multiple)
- Q: When a module has hundreds of exported items, how should the output be handled to remain readable? → A: Always display full output, letting users pipe to `less` or `grep` if needed, or use the filter flags to reduce output
- Q: How should re-exported modules be handled in the tree display? → A: Show the original source module for each item (e.g., "decode (from Data.Aeson.Parser) :: ...")
- Q: When the tool encounters network errors or Hackage is unreachable, what should happen to cached data? → A: Fall back to cached data (even if expired) with warning message "⚠ Using cached data (network unavailable)"
- Q: What should happen when a package exists on Hackage but has no Haddock documentation uploaded? → A: Display available metadata (name, versions, synopsis, dependencies) with notice "⚠ Documentation not available"
- Q: Should Haddock comments/documentation be displayed by default, or controlled by a flag? → A: Add optional --with-comments flag to include Haddock documentation text; by default show only type signatures and names for concise output

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Quick Package Lookup (Priority: P1)

A developer needs to quickly find documentation for a Haskell package without leaving their terminal. They want to search for a package by name and get immediate access to its documentation URL and basic information.

**Why this priority**: This is the core value proposition - enabling developers to stay in their workflow without context-switching to a browser. This single feature makes the tool immediately useful.

**Independent Test**: Can be fully tested by running the CLI with a package name (e.g., "aeson") and verifying it returns the Hackage documentation URL and package summary, delivering immediate value to developers who prefer terminal workflows.

**Acceptance Scenarios**:

1. **Given** the CLI tool is installed, **When** user runs the query command with a valid package name "aeson", **Then** the tool displays the package's Hackage URL, latest version, synopsis, and maintainer information
2. **Given** the user queries a package, **When** the package exists on Hackage, **Then** the documentation URL is displayed within 2 seconds
3. **Given** the user queries a package, **When** the package does not exist, **Then** the tool displays a clear "package not found" message with suggestions for similar package names

---

### User Story 2 - List Package Versions (Priority: P1)

A developer needs to see all available versions of a package to choose which version to explore or to check if a newer version is available.

**Why this priority**: Version management is fundamental to Haskell development. Developers need to quickly see what versions exist before diving into specific version documentation.

**Independent Test**: Can be tested by querying a package with a versions flag (e.g., `hdoc langchain-hs --list-versions`) and verifying it displays all available versions with release dates.

**Acceptance Scenarios**:

1. **Given** the user queries a package with the list-versions flag, **When** the package exists on Hackage, **Then** the tool displays all available versions sorted by release date (newest first)
2. **Given** the user lists versions, **When** multiple versions exist, **Then** each version shows its release date and indicates which is the latest stable version
3. **Given** the user lists versions for a non-existent package, **When** the query executes, **Then** the tool displays a "package not found" error with suggestions

---

### User Story 3 - Version-Specific Documentation (Priority: P2)

A developer working with a specific version of a package needs to access documentation for that exact version, not just the latest release.

**Why this priority**: Real-world projects often pin dependencies to specific versions. This enables developers to access historically accurate documentation matching their codebase.

**Independent Test**: Can be tested by querying a package with a version flag (e.g., `hdoc langchain-hs --version 2.0.0.0`) and verifying it returns version-specific documentation tree and module information.

**Acceptance Scenarios**:

1. **Given** the user specifies a package and version with --version flag, **When** that version exists on Hackage, **Then** the tool displays the complete tree view and module information specific to that version
2. **Given** the user specifies an invalid version, **When** the query executes, **Then** the tool lists available versions and suggests the closest match
3. **Given** the user queries without specifying a version, **When** the package has multiple versions, **Then** the tool defaults to the latest stable version
4. **Given** the user queries a specific module with --version flag, **When** the version and module exist, **Then** the tool displays version-specific source code and documentation for that module

---

### User Story 4 - Deep Module Inspection (Priority: P2)

A developer needs to understand the implementation details of a specific module within a package. They want to see full documentation, type signatures, and source code for all functions and types in that module.

**Why this priority**: Understanding implementation details is critical for learning how to use a library correctly, debugging issues, or evaluating whether a package meets requirements. This complements the tree overview with detailed inspection.

**Independent Test**: Can be tested by querying a specific module with package and module path (e.g., `hdoc langchain-hs --module Data.LangChain`) and verifying it displays complete function signatures, Haddock documentation, and source code implementations.

**Acceptance Scenarios**:

1. **Given** the user specifies a package and module path, **When** the module exists, **Then** the tool displays all exported functions with their type signatures and source code
2. **Given** the module contains data types, **When** the query executes, **Then** the tool displays complete type definitions including all constructors and record fields
3. **Given** the module contains type classes, **When** the query executes, **Then** the tool displays the class definition with all method signatures
4. **Given** the user queries a module with --filter-functions flag, **When** the module has functions, types, and classes, **Then** the tool displays only exported functions (no types or classes)
5. **Given** the user queries with multiple filter flags (--filter-functions --filter-types), **When** the query executes, **Then** the tool displays only the requested categories (functions and types, but not classes)
6. **Given** the user queries a module with --with-comments flag, **When** the module has Haddock documentation, **Then** the tool displays type signatures along with Haddock comments for each function/type/class
7. **Given** the user queries without --with-comments flag, **When** the query executes, **Then** the tool displays only names and type signatures (no Haddock documentation text)

---

### User Story 5 - Module Search Within Packages (Priority: P4)

A developer remembers a module name but not the package it belongs to. They need to search Hackage to find which package contains a specific module.

**Why this priority**: This is a specialized use case that addresses the common problem of "I know the module, but which package is it in?" Useful but not critical for initial release.

**Independent Test**: Can be tested by searching for a module name (e.g., "Data.Aeson") and verifying the tool returns the containing package name and documentation link.

**Acceptance Scenarios**:

1. **Given** the user searches for a module name, **When** the module exists in Hackage packages, **Then** the tool lists all packages containing that module with their versions
2. **Given** multiple packages contain the same module, **When** the search executes, **Then** packages are ranked by download count or popularity
3. **Given** the module name is not found, **When** the search executes, **Then** the tool suggests similar module names from available packages

---

### Edge Cases

- Network failure handling: When Hackage is unreachable (network timeout, DNS failure), the tool falls back to cached data (even if expired) and displays a warning message "⚠ Using cached data (network unavailable)". Only fails if no cache exists.
- How does the system handle packages with special characters or unusual naming?
- What happens when the user's terminal doesn't support UTF-8 encoding for package descriptions?
- How does the tool behave when rate-limited by Hackage's servers?
- Missing documentation handling: When a package exists but has no Haddock documentation uploaded, the tool displays available metadata (name, versions, synopsis, dependencies) with notice "⚠ Documentation not available".
- How are deprecated or outdated packages indicated?
- Large output handling: When a package has hundreds of modules or a module has hundreds of exported items, the tool displays the complete output without pagination or truncation. Users can pipe to `less`/`grep` or use filter flags (--filter-functions, --filter-types, --filter-classes) to manage output size.
- How does the tool handle modules with no source code available (only type signatures)?
- What happens when the user specifies an invalid module path for a package?
- Re-exported items: When displaying module contents, re-exported functions/types/classes show their original source module (e.g., "decode (from Data.Aeson.Parser) :: ByteString -> Maybe a") to provide accurate module structure information.
- What happens when source code contains non-UTF-8 characters or special formatting?
- What happens when a package has hundreds of versions (e.g., very active packages)?
- How does the tool handle querying a version that exists but has no documentation or source code available?
- What happens when the user specifies a version string in an invalid format?
- How are pre-release versions (alpha, beta, RC) distinguished from stable versions in listings?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST query Hackage's package database to retrieve package information
- **FR-002**: System MUST display package name, latest version, synopsis, maintainer, and documentation URL for queried packages
- **FR-003**: System MUST support listing all available versions of a package with release dates via a --list-versions flag
- **FR-004**: System MUST support querying specific package versions via a --version flag for all query types (package tree, module details)
- **FR-005**: System MUST provide clear error messages when packages are not found
- **FR-006**: System MUST suggest similar package names when exact matches fail
- **FR-007**: System MUST complete queries and display results within 2 seconds under normal network conditions
- **FR-008**: System MUST handle network errors gracefully by falling back to cached data (even if expired) with a warning message; only fail if no cache exists
- **FR-010**: System MUST support searching for packages by module name
- **FR-011**: System MUST display results in a readable terminal-friendly format with a single command invocation (no interactive REPL mode)
- **FR-012**: System MUST display package information as a hierarchical tree showing package → modules → exported functions, types, and classes, with each item showing its name and type signature (for functions) or type definition with constructors/methods (for data types and type classes)
- **FR-013**: System MUST support querying detailed information for specific modules by specifying both package name and module path, displaying function/type signatures and source code implementation
- **FR-014**: System MUST support optional output filtering flags (--filter-functions, --filter-types, --filter-classes) that can be combined to display only specific categories of module exports
- **FR-015**: System MUST support optional --with-comments flag to include Haddock documentation text alongside type signatures; by default displays only names and type signatures for concise output
- **FR-016**: System MUST cache query results locally for 24 hours to reduce repeated network requests
- **FR-017**: System MUST indicate when displaying cached vs. live data

### Key Entities

- **Package**: Represents a Haskell package on Hackage, with attributes: name, all available versions with release dates, synopsis, description, maintainer, homepage URL, documentation URL, upload date, dependencies, exported modules (version-specific)
- **Module**: Represents a Haskell module within a package, with attributes: qualified name, containing package, version, exported functions, exported types, exported type classes, documentation text
- **Function**: Represents an exported function in a module, with attributes: name, type signature, documentation (Haddock comments), source code implementation, original source module (for re-exported items)
- **Type**: Represents a data type or type alias in a module, with attributes: name, type definition, constructors (for ADTs), record fields (for records), documentation, original source module (for re-exported items)
- **Type Class**: Represents a type class in a module, with attributes: name, type parameters, constraints, method signatures, documentation, original source module (for re-exported items)
- **Query Result**: Represents the response to a user query, containing: matched packages/modules/items, search metadata (query time, cache status), suggested alternatives

## Assumptions

- Hackage provides a stable, publicly accessible API or data source for querying package information, module lists, and documentation
- Hackage provides access to source code for packages (via source distributions or documentation archives)
- The tool will run on standard terminal emulators with UTF-8 support
- Users have network connectivity to access Hackage
- Local cache storage is available in the user's home directory or system temp directory
- Cache validity period of 24 hours balances freshness with performance
- Similarity matching for package names uses common string distance algorithms (e.g., Levenshtein distance)
- The tool outputs complete results without pagination, following Unix philosophy of composability (users can pipe to `less`, `grep`, `head`, etc.)
- Users can manage large output via filtering flags (--filter-functions, --filter-types, --filter-classes) or standard Unix text processing tools
- By default, output is concise (names and type signatures only); users opt-in to verbose Haddock documentation via --with-comments flag for readability and performance

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can retrieve package documentation URLs in under 2 seconds for 95% of queries
- **SC-002**: Users successfully find the correct package on their first query attempt in 80% of cases
- **SC-003**: The tool handles network failures gracefully, with clear error messages, in 100% of failure scenarios
- **SC-004**: The tool's search suggestions lead to the correct package within 3 attempts in 90% of "not found" scenarios
- **SC-005**: Users spend 50% less time context-switching to browsers for documentation lookups compared to manual searches
