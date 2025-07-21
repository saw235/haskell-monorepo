# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell monorepo built with Bazel containing multiple applications, libraries, and a TypeScript task management tool. The project uses Stackage for Haskell dependency management and PNPM for NodeJS dependencies.

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
```

### Dependency Management
```bash
# Pin Haskell dependencies after adding to MODULE.bazel
bazel run @stackage-unpinned//:pin -- --upgrade-hackage

# Install task manager dependencies
bazel run -- @pnpm//:pnpm --dir $PWD/tools/mcp-shrimp-task-manager/ install

# Add NodeJS dependency to task manager
cd tools/mcp-shrimp-task-manager && pnpm add <package-name>
```

### Formatting
```bash
# Format Haskell code (use fourmolu or ormolu)
# Format Bazel files
buildifier BUILD.bazel

# Format TypeScript in task manager
cd tools/mcp-shrimp-task-manager && npx prettier --write .
```

## Architecture

### Directory Structure
- `haskell/app/` - Standalone executable applications, each with BUILD.bazel
- `haskell/libs/` - Shared libraries and reusable components
- `tools/mcp-shrimp-task-manager/` - TypeScript task management tool (Git subtree)
- `MODULE.bazel` - Bazel module configuration with Haskell dependencies
- `stackage_snapshot.json` - Pinned Haskell dependencies (auto-generated)

### Build System
- **Primary:** Bazel with rules_haskell
- **Haskell toolchain:** GHC 9.8.1 via Bazel-managed toolchains
- **Dependency resolution:** Stackage LTS snapshots for reproducible builds
- **Multi-language:** Integrated TypeScript/NodeJS builds via aspect_rules_js

### Key Patterns
- Each Haskell app/lib requires its own BUILD.bazel with haskell_binary/haskell_library
- Dependencies added via MODULE.bazel then referenced as @stackage//:package-name
- TypeScript project is self-contained with its own package.json and BUILD.bazel
- All dependencies managed through Bazel - no direct cabal/stack usage

## Adding Components

### New Haskell Application
1. Create `haskell/app/<name>/` directory
2. Add BUILD.bazel with haskell_binary rule
3. Add Main.hs and other source files
4. Reference dependencies as `@stackage//:package-name`

### New Haskell Library  
1. Create `haskell/libs/<name>/` directory
2. Add BUILD.bazel with haskell_library rule
3. Organize sources by module structure
4. Make public with `visibility = ["//visibility:public"]` if needed

### Adding Haskell Dependencies
1. Add `stack.package(name = "package-name")` to MODULE.bazel
2. Run `bazel run @stackage-unpinned//:pin -- --upgrade-hackage`
3. Reference in BUILD.bazel as `@stackage//:package-name`

## Web Scraping Standards
- Use `scalpel` library for all web scraping
- Check robots.txt and set descriptive User-Agent headers  
- Write unit tests with local HTML fixtures, not live requests

## Important Rules
- Never modify bazel-* directories (build artifacts)
- Never use cabal/stack directly - all through Bazel
- Never use npm/yarn in task manager - use PNPM only
- Never modify stackage_snapshot.json manually
- Always update BUILD.bazel when changing source files
- Format code with fourmolu/ormolu (Haskell) and prettier (TypeScript)