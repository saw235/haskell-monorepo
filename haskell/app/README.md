# Applications

This directory contains individual Haskell applications, each in their own subdirectory.

## Structure

```
haskell/app/
├── hello-world/          # Simple hello world application
│   ├── BUILD.bazel
│   └── Main.hs
├── calculator/           # Basic calculator application
│   ├── BUILD.bazel
│   └── Main.hs
├── calculator-mcp/       # Basic calculator MCP application
│   ├── BUILD.bazel
│   └── Main.hs
└── README.md            # This file
```

## Building Applications

To build a specific application:

```bash
# Build hello-world
bazel build //haskell/app/hello-world:hello-world

# Build calculator
bazel build //haskell/app/calculator:calculator

# Build calculator-mcp
bazel build //haskell/app/calculator-mcp:calculator-mcp
```

## Running Applications

To run a specific application:

```bash
# Run hello-world
bazel run //haskell/app/hello-world:hello-world

# Run calculator
bazel run //haskell/app/calculator:calculator

# Run calculator-mcp (this is an MCP server)
bazel run //haskell/app/calculator-mcp:calculator-mcp
```

## Adding New Applications

To add a new application:

1. Create a new directory: `mkdir haskell/app/your-app-name`
2. Create `Main.hs` with your application code
3. Create `BUILD.bazel` with the haskell_binary target:

```bzl
load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

haskell_binary(
    name = "your-app-name",
    srcs = [":Main.hs"],
    deps = ["//:base"],  # Add other dependencies as needed
)
```

4. Build and run: `bazel run //haskell/app/your-app-name:your-app-name` 