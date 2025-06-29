# Development Guidelines for AI Agents

This document provides project-specific rules for AI agents to follow when working on this repository.

## 1. Project Overview

- **Project:** Haskell monorepo built with Bazel.
- **Purpose:** Contains multiple Haskell applications and libraries, alongside a NodeJS-based task management tool.
- **Technology Stack:** Haskell, TypeScript/NodeJS, Bazel, Stackage, PNPM.

## 2. Project Architecture

- **`/` (Root):** Contains project-wide Bazel configurations (`MODULE.bazel`, `WORKSPACE`).
- **`haskell/app/`:** Location for all standalone Haskell applications. Each application **MUST** have its own `BUILD.bazel` file.
- **`haskell/libs/`:** Location for all shared Haskell libraries. Each library **MUST** have its own `BUILD.bazel` file.
- **`tools/mcp-shrimp-task-manager/`:** A self-contained TypeScript project managed as a Git subtree.
- **`bazel-*` directories:** Build artifacts. **MUST NOT be modified manually.**

## 3. Code Standards

### General
- All code, comments, and documentation **MUST** be in English.
- All files **MUST** use UTF-8 encoding.

### Haskell
- **Formatting:** Use `fourmolu` or `ormolu`. A root configuration file **MUST** be created (e.g., `.fourmolu.yaml`).
- **Naming Conventions:**
  - Modules: `PascalCase` (e.g., `My.Module.Name`)
  - Types and Type Classes: `PascalCase`
  - Functions and Variables: `camelCase`
- **Comments:** Use Haddock syntax (`-- |`) for all top-level definitions.

### TypeScript/NodeJS (`tools/mcp-shrimp-task-manager`)
- **Formatting:** Use `prettier`. A root `.prettierrc` configuration file **MUST** be created.
- **Naming Conventions:**
  - Files: `camelCase.ts`
  - Interfaces and Classes: `PascalCase`
  - Functions and Variables: `camelCase`
  - Constants: `UPPER_SNAKE_CASE`
- **Imports:** Group imports in the following order: external packages, project modules, relative imports.

### Bazel
- **Formatting:** Use `buildifier` to format all `BUILD.bazel` and `.bzl` files.

## 4. Functionality Implementation Standards

### Adding a New Haskell Application
1.  Create a new directory: `haskell/app/<new-app-name>`.
2.  Add a `BUILD.bazel` file defining a `haskell_binary` rule.
3.  Place source files (`Main.hs`, etc.) inside the new directory.
- **DO:** Ensure `bazel run //haskell/app/<new-app-name>:<new-app-name>` builds and runs the app.
- **DON'T:** Place application code inside `haskell/libs`.

### Adding a New Haskell Library
1.  Create a new directory: `haskell/libs/<new-lib-name>`.
2.  Add a `BUILD.bazel` file defining a `haskell_library` rule.
3.  Organize source files according to module names (e.g., `haskell/libs/new-lib/src/My/Lib.hs`).
- **DO:** Add the library as a dependency to other targets via its Bazel label (e.g., `deps = ["//haskell/libs/new-lib"]`).
- **DON'T:** Create circular dependencies between libraries.

### Adding a Web Scraper
- **Library:** All Haskell web scraping **MUST** use the `scalpel` library.
- **Responsibility:**
  - **DO:** Check and respect the target website's `robots.txt` before scraping.
  - **DO:** Set a descriptive `User-Agent` string in all HTTP requests to identify your scraper.
- **Testing:**
  - **DO:** Write unit tests for scraping logic using local HTML files as fixtures.
  - **DON'T:** Rely on live network requests in unit tests.

### Modifying `tools/mcp-shrimp-task-manager`
- All changes **MUST** be self-contained within this subtree.
- Add new dependencies using `pnpm add <package>`.
- Update `tools/mcp-shrimp-task-manager/BUILD.bazel` to reflect any changes in source files.

## 5. Dependency Management

### Haskell Dependencies
- **MUST** be managed by adding `stack.package` rules to `MODULE.bazel`.
- **To add a dependency:**
  1. Add a `stack.package(name = "<package-name>")` declaration to `MODULE.bazel`.
  2. If the default package from Stackage causes a build failure due to version conflicts (e.g., a dependency of a dependency requires a specific version), find the required version on Hackage, and specify it directly in the name. For example: `stack.package(name = "data-default-0.8.0.0")`. A package can be found usually via https://hackage-content.haskell.org/package/<package_name>
  3. Run `bazel run @stackage-unpinned//:pin -- --upgrade-hackage` to update `stackage_snapshot.json`.
- **To use a dependency:** Reference it in a `deps` attribute in a `BUILD.bazel` file using the `@stackage//:` prefix (e.g., `deps = ["@stackage//:attoparsec"]`).
- **DO NOT** modify `stackage_snapshot.json` manually.

### NodeJS Dependencies (`tools/mcp-shrimp-task-manager`)
- **MUST** be managed using `pnpm`.
- **To add a dependency:**
  1. `cd tools/mcp-shrimp-task-manager`
  2. `pnpm add <package-name>`
- Commit the updated `package.json` and `pnpm-lock.yaml` files.

## 6. Workflow and Key File Interactions

- **Workflow:**
  1. Create a feature branch from `main`.
  2. Make changes and update corresponding `BUILD.bazel` files.
  3. Run `bazel build //...` and `bazel test //...`.
  4. Format code with specified formatters.
  5. Commit and open a pull request.
- **Key File Interactions:**
  - **`MODULE.bazel`:** Modify when adding new Haskell dependencies, new Bazel toolchains, or external repositories.
  - **`stackage_snapshot.json`:** **DO NOT** modify this file manually. It is managed by `bazel run @stackage-unpinned//:pin`.
  - **`pnpm-lock.yaml`:** Modified automatically by `pnpm`. Commit the changes.

## 7. AI Decision-Making Standards

- **Rule Precedence:** These root-level rules override any conflicting rules in subdirectories.
- **Decision Tree for Code Changes:**
  1. **Identify Target:** Is it for a Haskell App, a Haskell Lib, or the TypeScript Tool?
  2. **Check Location:**
      - If it's a reusable component -> `haskell/libs`.
      - If it's a runnable executable -> `haskell/app`.
      - If it's for the task manager -> `tools/mcp-shrimp-task-manager`.
  3. **Update BUILD file:** Any change to source files requires updating the corresponding `BUILD.bazel` file's `srcs` or `deps`.
  4. **Check Dependencies:** If new dependencies are needed, follow the standards in Section 5.
  5. **Test:** Run `bazel build` and `bazel test` on all affected targets.

## 8. Prohibited Actions

- **DO NOT** modify files inside `bazel-*` directories.
- **DO NOT** use `cabal` or `stack` directly for Haskell package management.
- **DO NOT** use `npm` or `yarn` in `tools/mcp-shrimp-task-manager`.
- **DO NOT** commit directly to the `main` branch.
- **DO NOT** introduce circular dependencies between Bazel targets.
- **DO NOT** commit large binary files to the repository.
- **DO NOT** ignore linter or formatter errors.
- **DO NOT** modify the `tools/mcp-shrimp-task-manager` subtree unless the task is explicitly about that tool.
- **DO NOT** introduce new programming languages or major frameworks without a project-level decision. 