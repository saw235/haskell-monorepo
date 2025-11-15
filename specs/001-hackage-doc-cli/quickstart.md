# Quickstart: Hackage Documentation CLI

**Feature**: 001-hackage-doc-cli
**Date**: 2025-11-15
**For**: Developers implementing this feature

## Prerequisites

- Haskell toolchain with GHC 9.10.1
- Bazel 7.4.0
- All dependencies already in Stackage LTS-24.19 (no new packages needed!)
- Internet connection for Hackage API access

## Project Structure Overview

```
haskell/libs/hackage-client/          # Reusable library
haskell/app/hackage-doc-cli/          # CLI application
specs/001-hackage-doc-cli/            # This documentation
```

## 5-Minute Setup

### Step 1: Create Library Structure

```bash
cd $REPO_ROOT

# Create library directory
mkdir -p haskell/libs/hackage-client/HackageClient

# Create library modules
touch haskell/libs/hackage-client/HackageClient.hs
touch haskell/libs/hackage-client/HackageClient/Types.hs
touch haskell/libs/hackage-client/HackageClient/API.hs
touch haskell/libs/hackage-client/HackageClient/Parser.hs
touch haskell/libs/hackage-client/HackageClient/Cache.hs
touch haskell/libs/hackage-client/HackageClient/TreeDisplay.hs
```

### Step 2: Create Library BUILD.bazel

```bash
cat > haskell/libs/hackage-client/BUILD.bazel <<'EOF'
load("@rules_haskell//haskell:defs.bzl", "haskell_library")

haskell_library(
    name = "hackage-client",
    srcs = glob(["**/*.hs"]),
    visibility = ["//visibility:public"],
    deps = [
        "//:base",
        "//:aeson",
        "//:bytestring",
        "//:containers",
        "//:directory",
        "//:filepath",
        "//:http-conduit",
        "//:text",
        "//:time",
    ],
)
EOF
```

### Step 3: Create CLI Application Structure

```bash
# Create app directory
mkdir -p haskell/app/hackage-doc-cli/CLI

# Create CLI modules
touch haskell/app/hackage-doc-cli/Main.hs
touch haskell/app/hackage-doc-cli/CLI/Options.hs
touch haskell/app/hackage-doc-cli/CLI/Commands.hs
```

### Step 4: Create CLI BUILD.bazel

```bash
cat > haskell/app/hackage-doc-cli/BUILD.bazel <<'EOF'
load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

haskell_binary(
    name = "hackage-doc-cli",
    srcs = glob(["**/*.hs"]),
    deps = [
        "//:base",
        "//:optparse-applicative",
        "//:text",
        "//haskell/libs/hackage-client",
    ],
    visibility = ["//visibility:public"],
)
EOF
```

### Step 5: Create Test Structure

```bash
mkdir -p haskell/app/hackage-doc-cli/test/Fixtures
touch haskell/app/hackage-doc-cli/test/ParserSpec.hs
touch haskell/app/hackage-doc-cli/test/CacheSpec.hs
touch haskell/app/hackage-doc-cli/test/TreeDisplaySpec.hs
```

## Implementation Roadmap

### Phase 1: Core Types (Day 1)

**File**: `haskell/libs/hackage-client/HackageClient/Types.hs`

Implement data types from [data-model.md](./data-model.md):

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HackageClient.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime)
import GHC.Generics

data Package = Package
  { packageName :: Text
  , packageVersions :: [Version]
  , packageSynopsis :: Text
  -- ... (see data-model.md for full definition)
  } deriving (Show, Eq, Generic)

instance ToJSON Package
instance FromJSON Package

-- Implement other types: Version, Module, Function, Type, TypeClass, etc.
```

**Test**: Verify JSON serialization round-trips

---

### Phase 2: HTTP Client (Day 2)

**File**: `haskell/libs/hackage-client/HackageClient/API.hs`

Implement Hackage API client:

```haskell
module HackageClient.API where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL

-- Fetch package metadata from Hackage
fetchPackageInfo :: Text -> IO (Either String Package)
fetchPackageInfo pkgName = do
  let url = "https://hackage.haskell.org/package/" <> pkgName
  response <- httpLBS =<< parseRequest (Text.unpack url)
  return $ eitherDecode (getResponseBody response)

-- Fetch version list
fetchVersions :: Text -> IO (Either String [Version])
-- Fetch module documentation
fetchModuleDocs :: Text -> Text -> Text -> IO (Either String Module)
```

**Test**: Use local JSON fixtures from `test/Fixtures/`

---

### Phase 3: Caching Layer (Day 3)

**File**: `haskell/libs/hackage-client/HackageClient/Cache.hs`

Implement file-based cache:

```haskell
module HackageClient.Cache where

import System.Directory
import System.FilePath
import Data.Aeson

cacheDir :: IO FilePath
cacheDir = do
  base <- getXdgDirectory XdgCache "hackage-cli"
  createDirectoryIfMissing True base
  return base

readCache :: FromJSON a => String -> IO (Maybe (CacheEntry a))
readCache key = do
  dir <- cacheDir
  let path = dir </> key <> ".json"
  exists <- doesFileExist path
  if exists
    then decodeFileStrict path
    else return Nothing

writeCache :: ToJSON a => String -> a -> NominalDiffTime -> IO ()
writeCache key value ttl = do
  dir <- cacheDir
  now <- getCurrentTime
  let entry = CacheEntry value now ttl key
  let path = dir </> key <> ".json"
  encodeFile path entry
```

**Test**: Verify cache write, read, and TTL expiration

---

### Phase 4: Tree Display (Day 4)

**File**: `haskell/libs/hackage-client/HackageClient/TreeDisplay.hs`

Implement tree formatting:

```haskell
module HackageClient.TreeDisplay where

import Data.Tree
import qualified Data.Text as T

displayPackage :: Package -> String
displayPackage pkg = drawTree $ packageToTree pkg

packageToTree :: Package -> Tree String
packageToTree pkg = Node header moduleNodes
  where
    header = T.unpack (packageName pkg) <> " (" <> versionStr <> ")"
    versionStr = T.unpack (versionNumber $ head $ packageVersions pkg)
    moduleNodes = map moduleToTree (packageModules pkg)

moduleToTree :: Module -> Tree String
moduleToTree mod = Node modName functionNodes
  where
    modName = T.unpack (moduleName mod)
    functionNodes = map functionNode (moduleExportedFunctions mod)

functionNode :: Function -> Tree String
functionNode func = Node sig []
  where
    sig = T.unpack (functionName func) <> " :: " <> T.unpack (functionSignature func)
```

**Test**: Verify tree formatting with various package sizes

---

### Phase 5: CLI Interface (Day 5)

**File**: `haskell/app/hackage-doc-cli/CLI/Options.hs`

Implement option parsing:

```haskell
module CLI.Options where

import Options.Applicative

data Command
  = QueryPackage Text (Maybe Text)  -- package name, optional version
  | ListVersions Text                -- package name
  | QueryModule Text Text (Maybe Text)  -- package, module, optional version
  | SearchModule Text                -- module name
  | ClearCache

parseCommand :: Parser Command
parseCommand = subparser
  (  command "query" (info queryParser (progDesc "Query package"))
  <> command "versions" (info versionsParser (progDesc "List versions"))
  <> command "module" (info moduleParser (progDesc "Query module"))
  )
```

**File**: `haskell/app/hackage-doc-cli/Main.hs`

```haskell
module Main where

import Options.Applicative
import qualified HackageClient as HC
import CLI.Options
import CLI.Commands

main :: IO ()
main = do
  cmd <- execParser opts
  runCommand cmd
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
     <> progDesc "Query Hackage package documentation"
     <> header "hdoc - Hackage Documentation CLI" )
```

**Test**: Verify all command-line options parse correctly

---

## Build and Run

### Build the CLI

```bash
bazel build //haskell/app/hackage-doc-cli:hackage-doc-cli
```

### Run the CLI

```bash
bazel run //haskell/app/hackage-doc-cli:hackage-doc-cli -- aeson
```

### Run Tests

```bash
bazel test //haskell/app/hackage-doc-cli/test:all
```

## Development Workflow

### 1. Start with Types
- Implement all data types in `Types.hs`
- Add JSON instances
- Write QuickCheck properties for serialization

### 2. Build API Client
- Implement HTTP requests in `API.hs`
- Create test fixtures from real Hackage responses
- Test with local fixtures (no live requests in tests!)

### 3. Add Caching
- Implement cache read/write in `Cache.hs`
- Test TTL expiration logic
- Verify cache hit/miss scenarios

### 4. Implement Tree Display
- Build `Data.Tree` structures
- Format with type signatures
- Test with small, medium, large packages

### 5. Build CLI
- Parse command-line arguments
- Wire up library functions
- Add error handling and pretty output

### 6. Polish
- Improve error messages
- Add progress indicators
- Write documentation
- Format code with Ormolu

## Testing Strategy

### Unit Tests (HUnit)
```haskell
-- test/ParserSpec.hs
testPackageParse :: Assertion
testPackageParse = do
  json <- BL.readFile "test/Fixtures/aeson-package.json"
  let result = eitherDecode json :: Either String Package
  case result of
    Right pkg -> assertEqual "package name" "aeson" (packageName pkg)
    Left err -> assertFailure err
```

### Property Tests (QuickCheck)
```haskell
-- test/CacheSpec.hs
prop_cacheRoundTrip :: Package -> Property
prop_cacheRoundTrip pkg = ioProperty $ do
  writeCache "test-pkg" pkg 3600
  cached <- readCache "test-pkg"
  return $ cached === Just pkg
```

### Integration Tests
```bash
# Use local fixtures, no live Hackage requests
bazel test //haskell/app/hackage-doc-cli/test:all
```

## Debugging Tips

### Enable Verbose Logging
```haskell
-- Add to API.hs
import Debug.Trace (trace)

fetchPackageInfo pkg = trace ("Fetching: " <> pkg) $ do
  -- ... HTTP request
```

### Inspect Cache Files
```bash
# View cached data
cat ~/.cache/hackage-cli/pkg-aeson.json | jq .
```

### Test with curl
```bash
# Test Hackage API directly
curl https://hackage.haskell.org/package/aeson
```

## Common Issues

### Issue: Bazel can't find dependencies
**Solution**: All dependencies already in LTS-24.19, verify BUILD.bazel has correct `//:package-name` format

### Issue: JSON parsing fails
**Solution**: Check test fixtures match actual Hackage response format, update Types.hs if needed

### Issue: Cache not persisting
**Solution**: Check XDG directory creation, verify permissions on `~/.cache/`

### Issue: Tree display truncated
**Solution**: Large packages may exceed terminal width, implement pagination or truncation

## Next Steps After Implementation

1. Run `/speckit.tasks` to generate task breakdown
2. Follow task ordering (dependencies first)
3. Test each component independently
4. Integration test with real Hackage queries
5. Format code with Ormolu before committing
6. Create PR and request review

## Resources

- [Hackage API Documentation](https://hackage.haskell.org/api)
- [http-conduit Tutorial](https://github.com/snoyberg/http-client)
- [aeson Documentation](https://hackage.haskell.org/package/aeson)
- [optparse-applicative Guide](https://hackage.haskell.org/package/optparse-applicative)
- [Research Document](./research.md) - Detailed library choices and rationale
- [Data Model](./data-model.md) - Complete entity definitions
- [CLI Contract](./contracts/cli-interface.md) - Full interface specification

## Estimated Timeline

- **Setup**: 1 hour
- **Types**: 4 hours
- **API Client**: 6 hours
- **Caching**: 4 hours
- **Tree Display**: 4 hours
- **CLI Interface**: 4 hours
- **Tests**: 8 hours
- **Polish**: 3 hours

**Total**: ~34 hours (~1 week for one developer)
