---
name: hackage-read-docs
description: Retrieve documentation from Hackage. Use this Skill when you need a reference for Haskell packages, classes, or want to query package versions.
---

# Hackage Read Docs


## Quick start
# List all available versions
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --list-versions

# Query a specific version (shows tree view)
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --version 2.2.3.0

# Deep dive into a module
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --module Data.Aeson

## Instructions
1. Always start with listing the version first, pick the latest if it is unclear which to pick
2. Then query for the tree view to get the modules available in the package
3. Can deep dive into a module as needed