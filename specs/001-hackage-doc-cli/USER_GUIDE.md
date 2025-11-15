# Hackage Documentation CLI - User Guide

**hdoc** - Query Hackage package documentation from your terminal

## Quick Start

### Build the Tool

```bash
bazel build //haskell/app/hackage-doc-cli:hackage-doc-cli
```

### Basic Usage

All commands use `bazel run` with arguments after `--`:

```bash
# Query a package (shows tree view)
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson

# List all available versions
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --list-versions

# Query a specific version
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --version 2.2.3.0

# Deep dive into a module
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --module Data.Aeson
```

## Common Tasks

### Explore a Package

View the complete module hierarchy with type signatures:

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- lens
```

**Output:**
```
lens (5.2.3)
Synopsis: Lenses, Folds and Traversals
Docs: https://hackage.haskell.org/package/lens

Module tree:
Control.Lens
├── view :: MonadReader s m => Getting a s a -> m a
├── set :: ASetter s t a b -> b -> s -> t
└── over :: ASetter s t a b -> (a -> b) -> s -> t

Control.Lens.Getter
├── class Contravariant f => Gettable f where
│   └── coerce :: Gettable f => f a -> f b
...
```

### Check Package Versions

See all available versions sorted by release date:

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --list-versions
```

**Output:**
```
aeson - Available Versions

2.2.3.0    2024-06-15  [latest] [preferred]
2.2.2.0    2024-04-10
2.2.1.0    2024-02-20
...
Total: 55 versions
```

### Inspect a Specific Module

View full source code and documentation for a module:

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --module Data.Aeson
```

**Output:**
```
Module: Data.Aeson (aeson-2.2.3.0)

Functions:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
decode :: FromJSON a => ByteString -> Maybe a

Decode a JSON ByteString to a Haskell value.

Source:
decode = decodeStrict
{-# INLINE decode #-}
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
...
```

### Query Multiple Packages

Check several packages at once:

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson text bytestring
```

### Query from a File

Create a file with package names (one per line):

```bash
echo -e "aeson\ntext\nlens" > packages.txt
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- --from-file packages.txt
```

### Find a Module's Package

Don't know which package contains a module?

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- --search-module Data.Text
```

**Output:**
```
Packages containing module 'Data.Text':

1. text (2.0.2) - ★★★★★ (1.2M downloads/month)
2. text-short (0.1.5) - ★★☆☆☆ (15K downloads/month)
```

## Advanced Usage

### Version-Specific Module Inspection

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- \
  aeson --module Data.Aeson.Types --version 2.1.0.0
```

### Clear Cache

The tool caches results for faster subsequent queries. To clear the cache:

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- --clear-cache
```

### Get Help

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- --help
```

## Performance

- **First query**: ~2 seconds (fetches from Hackage)
- **Cached query**: <100ms
- **Batch queries**: ~10 seconds for 50 packages

Cache location: `~/.cache/hackage-cli/`
Cache validity: 24 hours for metadata, 30 days for immutable version data

## Troubleshooting

### "Package not found"

The tool will suggest similar package names:

```
Error: Package 'aesn' not found on Hackage.

Did you mean:
  - aeson
  - aeson-pretty
  - aeson-qq
```

### Network Issues

If you see network timeouts:
- Check your internet connection
- Hackage may be temporarily unavailable
- Try again in a few moments

```
Error: Network request timed out

Could not reach Hackage server. Check your internet connection.
```

### Module Not Found

If specifying a module that doesn't exist:

```
Error: Module 'Data.Aesn' not found in package 'aeson-2.2.3.0'.

Available modules:
  - Data.Aeson
  - Data.Aeson.Types
  - Data.Aeson.Parser
  ...
```

### Cache Issues

If you experience cache corruption or stale data:

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- --clear-cache
```

## Examples Workflow

**Scenario**: You want to use a JSON library in your project

1. **Find packages related to JSON:**
```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson
```

2. **Check available versions:**
```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson --list-versions
```

3. **Explore the API:**
```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- \
  aeson --module Data.Aeson --version 2.2.3.0
```

4. **Check related packages:**
```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- \
  aeson aeson-pretty aeson-qq
```

## Environment Variables

### HACKAGE_CLI_CACHE_DIR

Override the default cache directory:

```bash
export HACKAGE_CLI_CACHE_DIR=/tmp/hackage-cache
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson
```

### HACKAGE_API_URL

Use a Hackage mirror (for testing or when main site is down):

```bash
export HACKAGE_API_URL=https://hackage-mirror.example.com
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson
```

## Tips & Tricks

### Create a Shell Alias

For convenience, add to your `.bashrc` or `.zshrc`:

```bash
alias hdoc='bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli --'
```

Then use simply:
```bash
hdoc aeson
hdoc aeson --list-versions
```

### Combine with Other Tools

Pipe output to `less` for easier navigation:

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- lens | less
```

Search within output:

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson | grep -i "parse"
```

## Full Command Reference

See [contracts/cli-interface.md](./contracts/cli-interface.md) for complete CLI documentation including:
- All commands and flags
- Detailed examples
- Error codes
- Output formats

## For Developers

If you're contributing to or modifying this tool, see:
- [quickstart.md](./quickstart.md) - Implementation guide
- [data-model.md](./data-model.md) - Data structures
- [research.md](./research.md) - Technical decisions

## Support

- File issues on the project repository
- Check cache status with verbose flag (when implemented)
- Clear cache if experiencing issues
