# Haskell Monorepo

A monorepo containing multiple Haskell applications and libraries, built with Bazel and featuring a NodeJS-based task management tool.

## Project Overview

This repository is organized as a Bazel-based monorepo with the following structure:

- **Haskell Applications** (`haskell/app/`): Standalone executable applications
- **Haskell Libraries** (`haskell/libs/`): Shared libraries and reusable components
- **Electron Applications** (`electron-app/`): Cross-platform desktop applications with Haskell backends
- **Task Manager** (`tools/mcp-shrimp-task-manager/`): TypeScript/NodeJS-based task management tool
- **Build System**: Bazel with Stackage integration for Haskell dependency management

## Technology Stack

- **Build System**: Bazel
- **Haskell**: Managed via Stackage snapshots
- **TypeScript/NodeJS**: PNPM for dependency management
- **Code Formatting**: Fourmolu/Ormolu (Haskell), Prettier (TypeScript)

## Project Structure

```
├── haskell/
│   ├── app/           # Haskell applications (executables)
│   └── libs/          # Haskell libraries (shared code)
├── electron-app/      # Electron desktop applications
│   └── aivika-population-viz/  # Population growth visualization
├── tools/
│   └── mcp-shrimp-task-manager/  # TypeScript task manager
├── MODULE.bazel       # Bazel module configuration
├── WORKSPACE          # Bazel workspace configuration
└── stackage_snapshot.json  # Haskell dependency snapshot
```

## Development Setup

### Prerequisites

- Bazel
- Haskell toolchain (managed by Bazel)
- Node.js and PNPM (for task manager)

### Initial Setup

1. Clone the repository
2. Install task manager dependencies:
   ```bash
   bazel run -- @pnpm//:pnpm --dir $PWD/tools/mcp-shrimp-task-manager/ install
   ```

### Building and Testing

```bash
# Build all targets
bazel build //...

# Run all tests
bazel test //...

# Build a specific application
bazel build //haskell/app/<app-name>:<app-name>

# Run a specific application
bazel run //haskell/app/<app-name>:<app-name>
```

## Dependency Management

### Haskell Dependencies

Haskell dependencies are managed through Stackage snapshots. To add a new dependency:

1. Add the package to `MODULE.bazel`:
   ```bazel
   stack.package(name = "package-name")
   ```

2. Update the Stackage snapshot:
   ```bash
   bazel run @stackage-unpinned//:pin -- --upgrade-hackage
   ```

3. Use the dependency in your `BUILD.bazel`:
   ```bazel
   deps = ["@stackage//:package-name"]
   ```

### TypeScript Dependencies

For the task manager, dependencies are managed with PNPM:

```bash
cd tools/mcp-shrimp-task-manager
pnpm add <package-name>
```

## Code Standards

### Haskell
- Use Fourmolu or Ormolu for formatting
- Follow Haddock documentation standards
- PascalCase for modules, types, and type classes
- camelCase for functions and variables

### TypeScript
- Use Prettier for formatting
- Follow standard TypeScript naming conventions
- Group imports: external packages → project modules → relative imports

### Bazel
- Use Buildifier to format BUILD files
- Each application and library must have its own `BUILD.bazel` file

## Adding New Components

### New Haskell Application

1. Create directory: `haskell/app/<app-name>/`
2. Add `BUILD.bazel` with `haskell_binary` rule
3. Add source files (e.g., `Main.hs`)

### New Haskell Library

1. Create directory: `haskell/libs/<lib-name>/`
2. Add `BUILD.bazel` with `haskell_library` rule
3. Organize source files by module structure

### Web Scraping

- Use the `scalpel` library for all web scraping
- Respect `robots.txt` and set descriptive User-Agent headers
- Write unit tests with local HTML fixtures

## Running the Task Manager

To install the dependencies for the task manager, run the following command from the root of the workspace:

```bash
bazel run -- @pnpm//:pnpm --dir $PWD/tools/mcp-shrimp-task-manager/ install
``` 

## Aivika Population Growth Visualization

This project includes an interactive Electron application for visualizing population growth simulations powered by the Aivika system dynamics library.

### Features

- **Real-time WebSocket Communication**: Haskell backend with Electron frontend
- **Interactive Visualization**: Chart.js-powered population growth charts
- **Customizable Parameters**: Adjustable initial population, growth rate, and simulation time
- **Multiple Views**: Chart view, raw data table, and mathematical analysis
- **Data Export**: CSV and JSON export functionality
- **Mathematical Model**: Implements exponential growth (dP/dt = r × P)

### Running the Demo

To run the complete Aivika population growth visualization (both Haskell server and Electron frontend):

```bash
# Run the complete demo with both server and client
bazel run //electron-app/aivika-population-viz:aivika-demo

# Run in development mode with developer tools
bazel run //electron-app/aivika-population-viz:aivika-demo-dev
```

### Running Components Separately

You can also run the components individually:

```bash
# Run just the Haskell backend in CLI mode
bazel run //haskell/app/aivika-population-growth:aivika-population-growth

# Run the Haskell backend in WebSocket server mode
bazel run //haskell/app/aivika-population-growth:aivika-population-growth -- --server --port 9161

# Run just the Electron visualization app (requires running server separately)
bazel run //electron-app/aivika-population-viz:aivika-population-viz
```

### Environment Variables for CLI Mode

When running the Haskell backend in CLI mode, you can customize parameters:

```bash
INITIAL_POP=500 GROWTH_RATE=0.03 TIME_END=10 bazel run //haskell/app/aivika-population-growth:aivika-population-growth
```

## Pin Stackage Dependencies

To update and pin Haskell dependencies:

```bash
bazel run @stackage-unpinned//:pin
```

## Contributing

1. Create a feature branch from `main`
2. Make changes following the code standards
3. Update corresponding `BUILD.bazel` files
4. Run `bazel build //...` and `bazel test //...`
5. Format code with appropriate formatters
6. Commit and open a pull request

## Important Notes

- **DO NOT** modify files in `bazel-*` directories
- **DO NOT** use Cabal or Stack directly for Haskell package management
- **DO NOT** use npm or yarn in the task manager (use PNPM)
- **DO NOT** commit directly to the `main` branch
- **DO NOT** modify `stackage_snapshot.json` manually