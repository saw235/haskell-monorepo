# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a sophisticated Haskell monorepo built with Bazel containing multiple applications, libraries, TypeScript tools, and Electron desktop applications. The project demonstrates advanced polyglot development with unified dependency management across Haskell, TypeScript, and NodeJS ecosystems.

## Common Commands

### Build and Test
```bash
# Build all targets
bazel build //...

# Run all tests
bazel test //...

# Build specific application
bazel build //haskell/app/<app-name>:<app-name>

# Run specific application
bazel run //haskell/app/<app-name>:<app-name>

# Build specific library
bazel build //haskell/libs/<lib-name>

# Test with CI configuration (limited parallelism)
bazel test --config=ci //...

# Build with verbose failure output
bazel build --verbose_failures //...
```

### Dependency Management
```bash
# Pin Haskell dependencies after adding to MODULE.bazel
bazel run @stackage-unpinned//:pin -- --upgrade-hackage

# Install task manager dependencies
bazel run -- @pnpm//:pnpm --dir $PWD/tools/mcp-shrimp-task-manager/ install

# Install Electron app dependencies
cd electron-app/hello-world && pnpm install
cd electron-app/tic-tac-toe && pnpm install

# Add NodeJS dependency to task manager
cd tools/mcp-shrimp-task-manager && pnpm add <package-name>
```

### Development Environment
```bash
# Docker setup for web scraping apps
docker build -t haskell-scraper .
docker run -it --rm -v $(pwd):/workspace haskell-scraper

# Run Nike scraper with Selenium (requires Chrome/ChromeDriver)
bazel run //haskell/app/nike-scraper:nike-scraper-with-selenium

# Run Electron apps
cd electron-app/hello-world && npm start
cd electron-app/tic-tac-toe && npm start
```

### Formatting and Linting
```bash
# Format Haskell code (automated in CI)
find . -name "*.hs" -exec ormolu --mode inplace {} \;

# Format Bazel files
buildifier BUILD.bazel

# Format TypeScript in task manager
cd tools/mcp-shrimp-task-manager && npx prettier --write .
```

## Architecture

### Multi-Ecosystem Structure
- `haskell/app/` - 12 standalone applications (games, servers, scrapers, simulations)
- `haskell/libs/` - Shared libraries (antifingerprinting, mcp-server)
- `tools/mcp-shrimp-task-manager/` - Advanced TypeScript MCP server with i18n support
- `electron-app/` - Cross-platform desktop applications with Haskell backend integration
- `external/` - External dependencies (Selenium server, Chrome drivers)

### Advanced Build System
- **Primary:** Bazel 7.4.0 with Bzlmod enabled for modern dependency management
- **Haskell toolchain:** GHC 9.10.1 via Stackage LTS-24.19 snapshot
- **Multi-language:** Integrated TypeScript/NodeJS builds via aspect_rules_js
- **Platform support:** OS-specific configurations in .bazelrc for macOS/Windows/Linux
- **Custom extensions:** Non-module dependencies via non_module_deps.bzl
- **Container support:** Docker environment for web scraping with Chrome/Selenium

### Key Patterns and Conventions
- Each Haskell app/lib requires its own BUILD.bazel with haskell_binary/haskell_library
- Complex packages (zlib, attoparsec) use custom configurations in non_module_deps.bzl
- TypeScript uses multiple PNPM workspaces (task manager + 2 Electron apps)
- Web scraping standardized on scalpel library with Selenium WebDriver integration
- All dependencies managed through Bazel - no direct cabal/stack/npm usage
- Automated formatting enforced in CI pipeline

### NodeJS/TypeScript Ecosystem
Three separate PNPM workspaces:
1. **Task Manager** (`tools/mcp-shrimp-task-manager/`) - MCP server with sophisticated prompt templates
2. **Electron Hello World** (`electron-app/hello-world/`) - Simple desktop app
3. **Electron Tic-Tac-Toe** (`electron-app/tic-tac-toe/`) - Game with Haskell backend integration

## Development Tools

### GHCi REPL for Bazel Targets
- Investigate function types and details for any Bazel target using GHCi REPL
- Use the following command pattern:
  ```bash
  bazel run //path/to:target@repl -- -e ":type map"
  ```
- Examples:
  - Check function type: `bazel run //path/to:target@repl -- -e ":type map"`
  - Get function info: `bazel run //path/to:target@repl -- -e ":info length"`
  - Import libraries: `bazel run //path/to:target@repl -- -e "import Data.List"`
- Allows querying types, getting information, or investigating functions without entering interactive mode

## Adding Components

### New Haskell Application
1. Create `haskell/app/<name>/` directory
2. Add BUILD.bazel with haskell_binary rule and appropriate dependencies
3. Add Main.hs and module files following existing patterns
4. For complex apps, create subdirectories (src/, test/) and update BUILD.bazel accordingly
5. Reference dependencies as `//:base` instead of `@stackage//:base`

### New Haskell Library  
1. Create `haskell/libs/<name>/` directory with module structure
2. Add BUILD.bazel with haskell_library rule
3. Make public with `visibility = ["//visibility:public"]` if needed by apps
4. Add to other apps' dependencies in their BUILD.bazel files

### Adding Haskell Dependencies
1. For simple packages: Add `stack.package(name = "package-name")` to MODULE.bazel
2. For complex packages: Add configuration to non_module_deps.bzl
3. Run `bazel run @stackage-unpinned//:pin -- --upgrade-hackage`
4. Reference in BUILD.bazel as `//:package-name`

### New Electron Application
1. Create directory under `electron-app/<name>/`
2. Add package.json with electron dependencies
3. Create BUILD.bazel with appropriate NodeJS rules
4. Use PNPM for package management: `cd electron-app/<name> && pnpm install`

## Web Scraping and External Tools

### Standards and Requirements
- Use `scalpel` library for all HTML parsing and web scraping
- Set descriptive User-Agent headers and respect robots.txt
- Use Selenium WebDriver for dynamic content requiring JavaScript
- Docker environment available with Chrome and ChromeDriver pre-installed
- Write unit tests with local HTML fixtures, not live requests

### Selenium Integration
```bash
# Selenium server available as external dependency
bazel run @selenium//file:selenium-server-standalone-3.141.59.jar

# Chrome/ChromeDriver configured in Docker environment
# Use run_with_selenium.sh scripts for testing
```

## CI/CD Integration

### GitHub Actions Workflow
- **Trigger:** @claude mentions in issues, PRs, and comments
- **Permissions:** Full repository access for Claude Code automation
- **Model:** Claude Sonnet 4 by default (Opus 4 available via configuration)
- **Features:** CI result reading, pull request creation capabilities

### Automated Quality Checks
- **Formatting:** Ormolu (Haskell), Prettier (TypeScript), Buildifier (Bazel)
- **Testing:** Comprehensive test suite with QuickCheck property-based testing
- **Platform Testing:** Multi-OS support via Bazel platform-specific configs

## Important Rules and Constraints

### Build System
- Never modify bazel-* directories (build artifacts)
- Never use cabal/stack directly - all through Bazel
- Never modify stackage_snapshot.json manually - use pin command
- Always update BUILD.bazel when adding/removing source files
- Use platform-specific .bazelrc configurations for cross-platform compatibility

### Package Management
- Use PNPM only for NodeJS dependencies (never npm/yarn)
- Each NodeJS workspace manages its own dependencies independently
- Complex Haskell packages require configuration in non_module_deps.bzl
- Pin dependencies after any MODULE.bazel changes

### Development Workflow
- Format code before committing (enforced in CI)
- Use Docker environment for web scraping development
- Test Electron apps locally before committing
- Follow existing patterns for new components
- Respect the polyglot architecture - don't mix ecosystems inappropriately

### Dependency Installation
- Always run ```bazel run -- @pnpm//:pnpm --dir $PWD/path to target dir/ install``` to install npm packages

## Active Technologies
- Haskell with GHC 9.10.1 (project uses Stackage LTS-24.19) + http-conduit (2.3.9.1), aeson (2.2.3.0), optparse-applicative (already in project), containers (core), directory (core) (001-hackage-doc-cli)
- File-based JSON cache in ~/.cache/hackage-cli/ with TTL-based invalidation (24h metadata, 7d versions, 30d immutable) (001-hackage-doc-cli)
- Haskell GHC 9.10.1 (Stackage LTS-24.19) + langchain-hs (LLM integration, ReAct agents), monad-logger (logging), ansi-terminal (colorized output), aeson (JSON), http-conduit (web search), Rust FFI (tiktoken-rs or HuggingFace tokenizers) (002-agentic-framework)
- File-based (skill markdown files, logs to file system), no database required (002-agentic-framework)
- Haskell GHC 9.10.1 (Stackage LTS-24.19) + yaml (YAML frontmatter parsing), cmark (Markdown parsing, may migrate to commonmark-hs for extensions), aeson (JSON/YAML integration), containers (Map/Set for indexing), text, directory, filepath, optparse-applicative (already in project) (001-dnd-ruleset-query)
- File-based Markdown + YAML frontmatter storage, directory hierarchy for categories, git for version control (001-dnd-ruleset-query)
- Library-centric: rpg-ruleset-core (core types, parsers, query engine, validation) + rpg-ruleset-query CLI app (001-dnd-ruleset-query)
- Future Phase 2c: megaparsec for formula/condition DSL parsing with AST validation (001-dnd-ruleset-query)
