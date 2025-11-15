# CLI Interface Contract

**Feature**: 001-hackage-doc-cli
**Date**: 2025-11-15
**Purpose**: Define command-line interface contract for Hackage documentation CLI tool

## Command Structure

### Binary Name
`hdoc` (Hackage Documentation CLI)

### General Syntax
```bash
hdoc [PACKAGE] [OPTIONS]
hdoc --help
hdoc --version
```

## Commands and Flags

### 1. Query Package (Default Command)

Display hierarchical tree of package with modules, functions, and types.

**Syntax**:
```bash
hdoc PACKAGE [--version VERSION]
```

**Arguments**:
- `PACKAGE` (required): Package name (e.g., `aeson`, `text`, `lens`)

**Flags**:
- `--version VERSION` (optional): Query specific package version (e.g., `--version 2.2.3.0`)
  - Default: Latest stable version if not specified
  - Version format: Semantic versioning or PVP (e.g., `1.2.3.4`)

**Output**:
- Package metadata header (name, version, synopsis, maintainer, doc URL)
- Hierarchical tree showing:
  - Modules (qualified names)
  - Functions with type signatures
  - Data types with constructors
  - Type classes with methods
- Cache status indicator (`[cached: 2h ago]` or `[live]`)

**Example**:
```bash
$ hdoc aeson
```

**Success Output**:
```
aeson (2.2.3.0) [cached: 30m ago]
Synopsis: Fast JSON parsing and encoding
Maintainer: Adam Bergmark
Docs: https://hackage.haskell.org/package/aeson

Module tree:
Data.Aeson
├── decode :: FromJSON a => ByteString -> Maybe a
├── encode :: ToJSON a => a -> ByteString
├── eitherDecode :: FromJSON a => ByteString -> Either String a
└── data Value = Object | Array | String | Number | Bool | Null

Data.Aeson.Types
├── class ToJSON a where
│   └── toJSON :: a -> Value
├── class FromJSON a where
│   └── parseJSON :: Value -> Parser a
└── data Parser a = ...
```

**Error Output** (package not found):
```
Error: Package 'aesn' not found on Hackage.

Did you mean:
  - aeson
  - aeson-pretty
  - aeson-qq
```

**Exit Codes**:
- `0`: Success
- `1`: Package not found
- `2`: Network error
- `3`: Invalid arguments

---

### 2. List Package Versions

Display all available versions of a package with release dates.

**Syntax**:
```bash
hdoc PACKAGE --list-versions
```

**Arguments**:
- `PACKAGE` (required): Package name

**Flags**:
- `--list-versions` (required): Trigger version listing mode

**Output**:
- List of all versions sorted by release date (newest first)
- Each version shows: version number, release date, latest/preferred indicators

**Example**:
```bash
$ hdoc aeson --list-versions
```

**Success Output**:
```
aeson - Available Versions [cached: 1d ago]

2.2.3.0    2024-06-15  [latest] [preferred]
2.2.2.0    2024-04-10
2.2.1.0    2024-02-20
2.2.0.0    2023-12-15
2.1.2.1    2023-10-05
...
(50 more versions)

Total: 55 versions
Latest stable: 2.2.3.0
```

**Exit Codes**:
- `0`: Success
- `1`: Package not found
- `2`: Network error

---

### 3. Query Specific Module

Display detailed information for a specific module including full source code.

**Syntax**:
```bash
hdoc PACKAGE --module MODULE [--version VERSION]
```

**Arguments**:
- `PACKAGE` (required): Package name
- `MODULE` (required via flag): Fully qualified module name

**Flags**:
- `--module MODULE` (required): Module to inspect (e.g., `Data.Aeson`, `Data.Aeson.Types`)
- `--version VERSION` (optional): Package version (default: latest stable)

**Output**:
- Module header (package, version, module name)
- All exported functions with:
  - Type signature
  - Haddock documentation
  - Source code implementation
- All exported types with:
  - Full definition
  - Constructors/record fields
  - Documentation
- All exported type classes with:
  - Method signatures
  - Documentation

**Example**:
```bash
$ hdoc aeson --module Data.Aeson --version 2.2.3.0
```

**Success Output**:
```
Module: Data.Aeson (aeson-2.2.3.0) [cached: 2h ago]

Functions:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
decode :: FromJSON a => ByteString -> Maybe a

Decode a JSON ByteString to a Haskell value. Returns
Nothing if the input is not valid JSON or the parse fails.

Source:
decode = decodeStrict
{-# INLINE decode #-}

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
encode :: ToJSON a => a -> ByteString

Encode a Haskell value as a JSON ByteString.

Source:
encode = encodeBuilder . toEncoding
{-# INLINE encode #-}

Types:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
data Value
  = Object !Object
  | Array !Array
  | String !Text
  | Number !Scientific
  | Bool !Bool
  | Null

A JSON value represented as a Haskell datatype.
...
```

**Error Output** (module not found):
```
Error: Module 'Data.Aesn' not found in package 'aeson-2.2.3.0'.

Available modules:
  - Data.Aeson
  - Data.Aeson.Types
  - Data.Aeson.Parser
  - Data.Aeson.Encoding
  ...
```

**Exit Codes**:
- `0`: Success
- `1`: Package or module not found
- `2`: Network error
- `4`: Invalid module name format

---

### 4. Batch Query

Query multiple packages in a single invocation.

**Syntax**:
```bash
hdoc PACKAGE1 PACKAGE2 PACKAGE3 [OPTIONS]
hdoc --from-file FILE
```

**Arguments**:
- Multiple `PACKAGE` names separated by spaces

**Flags**:
- `--from-file FILE` (optional): Read package names from file (one per line)
- `--version VERSION` (optional): Not supported for batch mode (ignored)

**Output**:
- Sequentially display information for each package
- Separator between packages
- Summary at end (success count, failure count)

**Example**:
```bash
$ hdoc aeson text bytestring
```

**Success Output**:
```
[1/3] aeson
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
aeson (2.2.3.0)
...

[2/3] text
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
text (2.0.2)
...

[3/3] bytestring
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
bytestring (0.11.5.3)
...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Summary: 3 packages queried
✓ Success: 3
✗ Failed: 0
Total time: 4.2s
```

**With --from-file**:
```bash
$ cat packages.txt
aeson
text
bytestring

$ hdoc --from-file packages.txt
```

**Exit Codes**:
- `0`: All packages succeeded
- `1`: Some packages failed
- `2`: Network error
- `5`: File not found (for --from-file)

---

### 5. Search by Module Name

Find which package contains a specific module.

**Syntax**:
```bash
hdoc --search-module MODULE
```

**Flags**:
- `--search-module MODULE` (required): Module name to search for

**Output**:
- List of packages containing the module
- Sorted by download count/popularity
- Shows package name, version, and module name

**Example**:
```bash
$ hdoc --search-module Data.Text
```

**Success Output**:
```
Packages containing module 'Data.Text':

1. text (2.0.2) - ★★★★★ (1.2M downloads/month)
   https://hackage.haskell.org/package/text

2. text-short (0.1.5) - ★★☆☆☆ (15K downloads/month)
   https://hackage.haskell.org/package/text-short
```

**Exit Codes**:
- `0`: Module found
- `1`: Module not found
- `2`: Network error

---

## Global Flags

### --help
Display help information for the command.

```bash
$ hdoc --help
```

**Output**:
```
hdoc - Hackage Documentation CLI

Usage:
  hdoc PACKAGE [--version VERSION]          Query package tree
  hdoc PACKAGE --list-versions               List all versions
  hdoc PACKAGE --module MODULE [--version]   Query module details
  hdoc PACKAGE1 PACKAGE2 ...                 Batch query packages
  hdoc --from-file FILE                      Query from file
  hdoc --search-module MODULE                Find package by module
  hdoc --clear-cache                         Clear local cache
  hdoc --help                                Show this help
  hdoc --version                             Show version

Examples:
  hdoc aeson                                 Show aeson package tree
  hdoc aeson --list-versions                 List all aeson versions
  hdoc aeson --version 2.2.3.0               Show specific version
  hdoc aeson --module Data.Aeson             Inspect module details
  hdoc aeson text lens                       Query multiple packages
  hdoc --search-module Data.Aeson            Find package for module

Options:
  --version VERSION    Query specific package version
  --module MODULE      Inspect specific module with source code
  --list-versions      List all available versions
  --from-file FILE     Read package names from file
  --search-module MOD  Search for package containing module
  --clear-cache        Clear local cache directory
  --help               Show this help message
  --version            Show hdoc version
```

### --version
Display hdoc tool version.

```bash
$ hdoc --version
```

**Output**:
```
hdoc 0.1.0
Hackage Documentation CLI Tool
```

### --clear-cache
Clear the local cache directory.

```bash
$ hdoc --clear-cache
```

**Output**:
```
Clearing cache at ~/.cache/hackage-cli/
Removed 42 cached files (12.3 MB)
Cache cleared successfully.
```

---

## Environment Variables

### HACKAGE_CLI_CACHE_DIR
Override default cache directory location.

**Default**: `~/.cache/hackage-cli/` (Linux/macOS), `%LOCALAPPDATA%\hackage-cli\cache` (Windows)

**Example**:
```bash
export HACKAGE_CLI_CACHE_DIR=/tmp/hackage-cache
hdoc aeson
```

### HACKAGE_API_URL
Override Hackage API base URL (for testing/mirrors).

**Default**: `https://hackage.haskell.org`

**Example**:
```bash
export HACKAGE_API_URL=https://hackage-mirror.example.com
hdoc aeson
```

---

## Performance Characteristics

### Response Time Targets (from spec.md)
- Single package query: < 2 seconds (95th percentile)
- Batch query (50 packages): < 10 seconds total
- Cache hit: < 100ms
- Version listing: < 1 second

### Network Requests
- Package query: 1-2 HTTP requests (metadata + optional tarball)
- Version listing: 1 HTTP request
- Module query: 1-2 HTTP requests (Haddock HTML + optional source)
- Batch query: N HTTP requests (sequential)

---

## Error Handling

### Error Message Format
```
Error: [Brief description]

[Optional: Detailed explanation or suggestions]
```

### Common Error Scenarios

#### Network Timeout
```
Error: Network request timed out

Could not reach Hackage server. Check your internet connection.
```

#### Invalid Package Name
```
Error: Invalid package name 'Aeson'

Package names must be lowercase. Did you mean 'aeson'?
```

#### Invalid Version Format
```
Error: Invalid version format '2.2.a'

Version must follow semantic versioning (e.g., 2.2.3.0)
```

#### Rate Limiting
```
Error: Rate limited by Hackage

Too many requests. Please wait 60 seconds before retrying.
```

---

## Testing Contract

### Unit Tests
- Argument parsing: Test all flag combinations
- Error messages: Verify format and suggestions
- Exit codes: Verify correct codes for each scenario

### Integration Tests (with fixtures)
- Mock Hackage responses with local HTML/JSON files
- Test tree display formatting
- Test cache hit/miss scenarios
- Test TTL expiration

### Property-Based Tests (QuickCheck)
- Package names: Valid characters, case handling
- Version parsing: Semantic versioning compliance
- Tree display: Consistent formatting regardless of tree depth
