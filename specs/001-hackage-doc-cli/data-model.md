# Data Model: Hackage Documentation CLI

**Feature**: 001-hackage-doc-cli
**Date**: 2025-11-15
**Source**: Derived from [spec.md](./spec.md) Key Entities section

## Core Entities

### Package

Represents a Haskell package on Hackage with all its versions and metadata.

```haskell
data Package = Package
  { packageName :: Text
  , packageVersions :: [Version]
  , packageSynopsis :: Text
  , packageDescription :: Text
  , packageMaintainer :: Text
  , packageHomepage :: Maybe Text
  , packageDocUrl :: Text
  , packageUploadDate :: UTCTime
  , packageDependencies :: [Dependency]
  , packageModules :: [Module]  -- version-specific
  } deriving (Show, Eq, Generic)

instance ToJSON Package
instance FromJSON Package
```

**Validation Rules**:
- `packageName` must be non-empty and match Hackage naming conventions (lowercase, hyphens allowed)
- `packageVersions` must contain at least one version
- `packageVersions` should be sorted newest-first for display
- `packageDocUrl` format: `https://hackage.haskell.org/package/{name}`

**Relationships**:
- One Package has many Versions
- One Package has many Modules (version-specific)
- One Package has many Dependencies

### Version

Represents a specific version of a package with release metadata.

```haskell
data Version = Version
  { versionNumber :: Text  -- e.g., "2.2.3.0"
  , versionReleaseDate :: UTCTime
  , versionIsPrerelease :: Bool  -- alpha, beta, RC
  , versionIsLatest :: Bool
  , versionIsPreferred :: Bool  -- Hackage preferred version
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Version
instance FromJSON Version
```

**Validation Rules**:
- `versionNumber` must follow semantic versioning or PVP (Package Versioning Policy)
- Only one version can have `versionIsLatest = True`
- `versionIsPrerelease` determined by version suffix (alpha, beta, rc, pre)

**State Transitions**:
- New version: `isLatest = False, isPreferred = False`
- On release: May become `isLatest = True` (previous latest becomes False)
- Maintainer action: May toggle `isPreferred`

### Module

Represents a Haskell module within a specific package version.

```haskell
data Module = Module
  { moduleName :: Text  -- qualified name, e.g., "Data.Aeson.Types"
  , modulePackage :: Text  -- package name
  , moduleVersion :: Text  -- version number
  , moduleExportedFunctions :: [Function]
  , moduleExportedTypes :: [Type]
  , moduleExportedClasses :: [TypeClass]
  , moduleDocumentation :: Maybe Text  -- Haddock module-level docs
  , moduleSourceUrl :: Maybe Text  -- link to source code
  } deriving (Show, Eq, Generic)

instance ToJSON Module
instance FromJSON Module
```

**Validation Rules**:
- `moduleName` must be valid Haskell module name (capitalized segments, dots as separators)
- At least one of `moduleExportedFunctions`, `moduleExportedTypes`, or `moduleExportedClasses` should be non-empty
- `moduleSourceUrl` format: `https://hackage.haskell.org/package/{package}-{version}/docs/src/{ModulePath}.html`

**Relationships**:
- One Module belongs to one Package Version
- One Module has many Functions
- One Module has many Types
- One Module has many TypeClasses

### Function

Represents an exported function in a module.

```haskell
data Function = Function
  { functionName :: Text
  , functionSignature :: Text  -- full type signature
  , functionDocumentation :: Maybe Text  -- Haddock comments
  , functionSourceCode :: Maybe Text  -- implementation source
  } deriving (Show, Eq, Generic)

instance ToJSON Function
instance FromJSON Function
```

**Validation Rules**:
- `functionName` must be valid Haskell identifier (lowercase start, alphanumeric + underscore)
- `functionSignature` must include :: separator
- `functionSourceCode` may be Nothing if source unavailable

**Display Format** (for tree view):
```
functionName :: TypeSignature
```

### Type

Represents a data type or type alias in a module.

```haskell
data Type = Type
  { typeName :: Text
  , typeDefinition :: Text  -- full definition (data/newtype/type)
  , typeConstructors :: [Constructor]  -- for ADTs
  , typeRecordFields :: [RecordField]  -- for records
  , typeDocumentation :: Maybe Text
  , typeKind :: TypeKind
  } deriving (Show, Eq, Generic)

data TypeKind = DataType | NewType | TypeAlias | TypeFamily
  deriving (Show, Eq, Generic)

data Constructor = Constructor
  { constructorName :: Text
  , constructorFields :: [Text]  -- field types
  } deriving (Show, Eq, Generic)

data RecordField = RecordField
  { recordFieldName :: Text
  , recordFieldType :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Type
instance FromJSON Type
instance ToJSON TypeKind
instance FromJSON TypeKind
instance ToJSON Constructor
instance FromJSON Constructor
instance ToJSON RecordField
instance FromJSON RecordField
```

**Validation Rules**:
- `typeName` must be valid Haskell type identifier (uppercase start)
- `typeConstructors` non-empty for DataType/NewType, empty for TypeAlias
- `typeRecordFields` populated only for record syntax
- `typeDefinition` must be valid Haskell syntax

**Display Format** (for tree view):
```
data TypeName = Constructor1 | Constructor2 { field :: Type }
```

### TypeClass

Represents a type class in a module.

```haskell
data TypeClass = TypeClass
  { typeClassName :: Text
  , typeClassParams :: [Text]  -- type parameters
  , typeClassConstraints :: [Text]  -- superclass constraints
  , typeClassMethods :: [Method]
  , typeClassDocumentation :: Maybe Text
  } deriving (Show, Eq, Generic)

data Method = Method
  { methodName :: Text
  , methodSignature :: Text
  , methodDefaultImpl :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON TypeClass
instance FromJSON TypeClass
instance ToJSON Method
instance FromJSON Method
```

**Validation Rules**:
- `typeClassName` must be valid Haskell type class name (uppercase start)
- `typeClassParams` must contain at least one parameter
- `typeClassMethods` should be non-empty (though technically allowed to be empty)

**Display Format** (for tree view):
```
class Constraint => ClassName param where
  method :: Signature
```

### Dependency

Represents a package dependency relationship.

```haskell
data Dependency = Dependency
  { depPackageName :: Text
  , depVersionConstraint :: Text  -- e.g., ">=1.0 && <2.0"
  } deriving (Show, Eq, Generic)

instance ToJSON Dependency
instance FromJSON Dependency
```

### QueryResult

Represents the response to a user query with metadata.

```haskell
data QueryResult a = QueryResult
  { queryData :: a  -- Package, [Package], Module, etc.
  , queryMetadata :: QueryMetadata
  , querySuggestions :: [Text]  -- alternative suggestions on partial matches
  } deriving (Show, Eq, Generic, Functor)

data QueryMetadata = QueryMetadata
  { queryTime :: UTCTime
  , queryDuration :: NominalDiffTime  -- execution time
  , queryCacheHit :: Bool  -- whether result was cached
  , queryCacheAge :: Maybe NominalDiffTime  -- age of cached data
  } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (QueryResult a)
instance FromJSON a => FromJSON (QueryResult a)
instance ToJSON QueryMetadata
instance FromJSON QueryMetadata
```

### CacheEntry

Represents a cached query result with TTL.

```haskell
data CacheEntry a = CacheEntry
  { cacheData :: a
  , cacheTimestamp :: UTCTime
  , cacheTTL :: NominalDiffTime  -- seconds until expiration
  , cacheKey :: Text  -- unique key for this cache entry
  } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (CacheEntry a)
instance FromJSON a => FromJSON (CacheEntry a)

-- Check if cache entry is still valid
isCacheValid :: CacheEntry a -> UTCTime -> Bool
isCacheValid entry now =
  diffUTCTime now (cacheTimestamp entry) < cacheTTL entry
```

**Cache Key Format**:
- Package metadata: `pkg:{packageName}`
- Version list: `versions:{packageName}`
- Package tree: `tree:{packageName}-{version}`
- Module details: `module:{packageName}-{version}-{moduleName}`

**TTL Strategy**:
- Package metadata: 86400 seconds (24 hours)
- Version lists: 604800 seconds (7 days)
- Package trees: 2592000 seconds (30 days, immutable)
- Module details: 2592000 seconds (30 days, immutable)

## Data Flow

### Package Query Flow
```
User Input (package name)
  ↓
Check Cache (pkg:{name})
  ↓ [cache miss]
Fetch from Hackage API
  ↓
Parse JSON Response
  ↓
Build Package Entity
  ↓
Write to Cache
  ↓
Return QueryResult<Package>
```

### Module Detail Query Flow
```
User Input (package, version, module)
  ↓
Check Cache (module:{pkg}-{ver}-{mod})
  ↓ [cache miss]
Fetch Haddock HTML or Tarball
  ↓
Parse HTML/Source
  ↓
Build Module Entity with Functions/Types
  ↓
Write to Cache
  ↓
Return QueryResult<Module>
```

## Persistence

### Cache File Structure
```
~/.cache/hackage-cli/
├── pkg-aeson.json              # Package metadata cache
├── versions-aeson.json         # Version list cache
├── tree-aeson-2.2.3.0.json    # Package tree cache
└── module-aeson-2.2.3.0-Data.Aeson.json  # Module detail cache
```

### JSON Schema Example (Package)
```json
{
  "cacheData": {
    "packageName": "aeson",
    "packageVersions": [
      {
        "versionNumber": "2.2.3.0",
        "versionReleaseDate": "2024-06-15T10:30:00Z",
        "versionIsPrerelease": false,
        "versionIsLatest": true,
        "versionIsPreferred": true
      }
    ],
    "packageSynopsis": "Fast JSON parsing and encoding",
    "packageDescription": "...",
    "packageMaintainer": "Adam Bergmark",
    "packageHomepage": "https://github.com/haskell/aeson",
    "packageDocUrl": "https://hackage.haskell.org/package/aeson",
    "packageUploadDate": "2024-06-15T10:30:00Z",
    "packageDependencies": [],
    "packageModules": []
  },
  "cacheTimestamp": "2025-11-15T14:30:00Z",
  "cacheTTL": 86400,
  "cacheKey": "pkg:aeson"
}
```

## Data Volume Estimates

- **Small package** (e.g., `mtl`): ~10 modules, ~100 functions, ~20 types → ~50KB JSON
- **Medium package** (e.g., `text`): ~50 modules, ~500 functions, ~50 types → ~200KB JSON
- **Large package** (e.g., `lens`): ~200 modules, ~2000 functions, ~500 types → ~1MB JSON
- **Expected cache size**: 10-50 packages cached = 5-50MB total

## Indexing Strategy

No database indexing required (file-based cache with direct key lookup).

**Lookup Performance**:
- Cache hit: O(1) file path construction + O(n) JSON parse where n = file size
- Cache miss: O(network) + O(parse) + O(write)

## Concurrency

Single-user CLI tool, no concurrent access to cache files expected.
If needed, implement file locking using `lukko` package (in LTS-24.19).
