# Research: Hackage Documentation CLI Tool

**Date**: 2025-11-15
**Feature**: 001-hackage-doc-cli
**Purpose**: Resolve technical unknowns and select appropriate libraries/approaches

## 1. Hackage API Access

### Decision
Use official Hackage JSON REST API at `https://hackage.haskell.org/api`

### Rationale
- **Official and stable**: Well-documented, maintained by Hackage team
- **No HTML scraping**: Clean JSON responses, easier to parse and maintain
- **Versioned**: Stable API contract, backward compatibility
- **Comprehensive**: Covers all required functionality (packages, versions, metadata, documentation)

### Key Endpoints
- `GET /packages/` - List all packages (paginated)
- `GET /package/:package` - Package metadata with all versions
- `GET /package/:package/:version/docs` - Haddock documentation archive
- `GET /package/:package/preferred` - Preferred/latest version information
- `GET /package/:package/:version/:module.html` - Module documentation (HTML)
- Source code access via tarball: `/package/:package-:version/:package-:version.tar.gz`

### Alternatives Considered
- **hackage-db package**: Not available in LTS-24.19, would require manual addition
- **HTML scraping**: Fragile, breaks with Hackage UI changes, maintenance burden
- **Tarball-only approach**: Requires full download and extraction, slower initial queries
- **Hoogle API**: Limited to search, doesn't provide comprehensive package structure

### Implementation Notes
- Use JSON API for metadata queries (fast, cacheable)
- Download Haddock HTML for detailed module information when needed
- Consider tarball download for source code extraction (gzip decompression required)

## 2. HTTP Client Library

### Decision
Use `http-conduit` (version 2.3.9.1) with `Network.HTTP.Simple` module

### Rationale
- **Already in LTS-24.19**: Zero new dependencies
- **Already used in project**: Referenced in `haskell/app/langchain-example/Main.hs`
- **Recommended by FP Complete**: Official documentation recommends for most users
- **Excellent error handling**: Strong exception types, timeout support
- **HTTPS built-in**: Secure connections with minimal configuration
- **Streaming support**: Can handle large tarball downloads efficiently

### Code Example
```haskell
import Network.HTTP.Simple

fetchPackageInfo :: String -> IO (Response ByteString)
fetchPackageInfo pkgName = do
  let url = "https://hackage.haskell.org/package/" <> pkgName
  httpBS =<< parseRequest url
```

### Alternatives Considered
- **req**: Not in LTS-24.19, would require MODULE.bazel modification
- **wreq**: Deprecated, maintainer inactive since 2018, poor error messages
- **http-client**: Lower-level, requires more boilerplate (http-conduit wraps this)

### Performance Characteristics
- Connection pooling: Built-in via Manager
- Request timeout: Configurable (default 5 seconds)
- Retry logic: Manual implementation needed
- Memory: Efficient ByteString handling

## 3. JSON/HTML Parsing

### Decision: JSON
Use `aeson` (version 2.2.3.0)

### Rationale
- **De facto standard**: Industry-standard JSON library for Haskell
- **Already in project**: Extensively used, already in MODULE.bazel
- **Generic deriving**: Automatic ToJSON/FromJSON instances
- **File I/O support**: Built-in `decodeFileStrict` for cache reading
- **Excellent performance**: Optimized for speed and memory

### Code Example
```haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson
import GHC.Generics

data PackageInfo = PackageInfo
  { name :: Text
  , versions :: [Text]
  , synopsis :: Text
  } deriving (Generic, Show)

instance FromJSON PackageInfo
instance ToJSON PackageInfo
```

### Decision: HTML (if needed)
Use existing `scalpel` library (already in project for web scraping)

### Rationale
- **Already available**: Used in nike-scraper and other apps
- **High-level API**: Easy extraction with CSS selectors
- **Works well for Haddock**: Can extract function signatures, documentation

### Alternatives Considered
- **tagsoup**: Lower-level, more boilerplate required
- **html-conduit**: Heavy dependency (pulls in xml-conduit), overkill

## 4. Tree Display

### Decision
Use `Data.Tree.drawTree` from `containers` package (core GHC library)

### Rationale
- **Zero dependencies**: Part of GHC base distribution (containers 0.6.7)
- **Perfect fit**: Designed exactly for ASCII tree rendering
- **Standard solution**: Well-tested, widely used, stable API
- **Flexible**: Easy to customize with fmap before rendering

### Code Example
```haskell
import Data.Tree

displayPackageTree :: Package -> String
displayPackageTree pkg = drawTree $ packageToTree pkg
  where
    packageToTree p = Node (packageName p) (map moduleToTree $ modules p)
    moduleToTree m = Node (moduleName m) (map funcNode $ functions m)
    funcNode f = Node (functionSignature f) []
```

### Output Example
```
aeson
├── Data.Aeson
│   ├── decode :: FromJSON a => ByteString -> Maybe a
│   ├── encode :: ToJSON a => a -> ByteString
│   └── eitherDecode :: FromJSON a => ByteString -> Either String a
└── Data.Aeson.Types
    ├── Parser :: Type -> Type
    └── parse :: (a -> Parser b) -> a -> Result b
```

### Alternatives Considered
- **pretty-tree**: Not in LTS-24.19, would need manual addition
- **tree-view**: Not in LTS-24.19, more complex than needed
- **boxes**: Over-engineering, designed for 2D layout not trees
- **Custom implementation**: Reinventing the wheel, maintenance burden

## 5. File-Based Caching

### Decision
Simple JSON file cache using `aeson` + `directory` package

### Rationale
- **Leverages existing dependencies**: aeson already used, directory is core library (1.3.8.5)
- **Human-readable**: JSON format allows manual inspection and debugging
- **Simple implementation**: ~50 lines of code, easy to maintain
- **Cross-platform**: directory package handles Windows/Unix path differences
- **No schema migration**: JSON flexibility handles evolving data structures

### Implementation Strategy
```haskell
-- Cache location: ~/.cache/hackage-cli/
-- File naming: {package-name}-{version}.json
-- TTL: 24 hours for metadata, longer for immutable version data

import System.Directory (getXdgDirectory, XdgDirectory(..), createDirectoryIfMissing)
import Data.Aeson (encodeFile, decodeFileStrict)
import System.FilePath ((</>))
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

cacheDir :: IO FilePath
cacheDir = do
  base <- getXdgDirectory XdgCache "hackage-cli"
  createDirectoryIfMissing True base
  return base

cacheFilePath :: String -> String -> IO FilePath
cacheFilePath pkgName version = do
  dir <- cacheDir
  return $ dir </> pkgName <> "-" <> version <> ".json"

writeCache :: ToJSON a => String -> String -> a -> IO ()
writeCache pkg ver dat = do
  path <- cacheFilePath pkg ver
  encodeFile path dat

readCache :: FromJSON a => String -> String -> IO (Maybe a)
readCache pkg ver = do
  path <- cacheFilePath pkg ver
  decodeFileStrict path
```

### TTL-Based Invalidation
- Metadata cache: 24 hours (may change frequently)
- Version lists: 7 days (new versions released occasionally)
- Immutable data (specific version documentation): 30 days (never changes)

### Alternatives Considered
- **cache library**: In-memory only, not persistent across sessions
- **cereal/binary**: Binary format not human-readable, harder debugging
- **SQLite**: Over-engineering, requires sqlite3 system dependency
- **Redis/external cache**: Requires separate service, deployment complexity

## 6. Additional Libraries Needed

### Command-Line Parsing
**Decision**: Use `optparse-applicative` (already in project via langchain-example)

**Rationale**:
- Already in MODULE.bazel
- Excellent help generation
- Type-safe option parsing
- Composable parsers

### Text Processing
**Decision**: Use `text` package (core library, version 2.0.2)

**Rationale**:
- Standard for Unicode text in Haskell
- Efficient operations
- Part of base dependencies

### Additional Core Libraries Required (all in GHC base)
- `filepath` (1.4.200.1) - Path manipulation
- `time` (1.12.2) - Timestamps for cache TTL
- `bytestring` (0.11.5.3) - Binary data handling

## Summary: Zero New Dependencies Required

**Excellent outcome**: All functionality can be implemented using packages that are either:
1. Already in MODULE.bazel (`http-conduit`, `aeson`, `optparse-applicative`)
2. Part of GHC core libraries (`containers`, `directory`, `filepath`, `time`, `bytestring`, `text`)

**No MODULE.bazel changes needed** - Can start implementation immediately!

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Hackage API changes | Low | High | Version endpoints, cache responses, monitor API docs |
| Rate limiting | Medium | Medium | Implement caching, exponential backoff, respect 429 responses |
| Large package downloads | Medium | Low | Stream tarballs, show progress, implement size limits |
| Cache corruption | Low | Low | Validate JSON on read, re-fetch on parse failure |
| Dependency version conflicts | Very Low | Low | All dependencies verified in LTS-24.19 |

## Implementation Roadmap

### Phase 1: Core HTTP + JSON (P1)
- Implement Hackage API client
- Parse package metadata JSON
- Basic error handling

### Phase 2: Caching Layer (P1)
- Implement cache directory structure
- TTL-based cache invalidation
- Cache read/write with aeson

### Phase 3: Tree Display (P2)
- Build Data.Tree structures from package data
- Format with type signatures
- Handle large packages gracefully

### Phase 4: CLI Interface (P1)
- Parse command-line arguments with optparse-applicative
- Implement --list-versions flag
- Implement --version flag
- Implement --module flag

### Phase 5: Polish (P3)
- Error messages
- Progress indicators
- Documentation
- Tests with fixtures

## Next Steps

1. Create `haskell/app/hackage-doc-cli/BUILD.bazel`
2. Create `haskell/libs/hackage-client/BUILD.bazel`
3. Implement HackageClient.API module (HTTP requests)
4. Implement HackageClient.Types module (data structures)
5. Implement HackageClient.Cache module (file caching)
6. Implement TreeDisplay module
7. Implement CLI Main.hs with optparse-applicative
8. Create test fixtures from real Hackage responses
9. Write QuickCheck properties and unit tests
