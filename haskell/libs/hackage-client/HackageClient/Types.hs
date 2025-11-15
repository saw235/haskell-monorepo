{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HackageClient.Types
  ( -- * Core Entities
    Package (..),
    Version (..),
    Module (..),
    Function (..),
    Type (..),
    TypeClass (..),
    Dependency (..),
    QueryResult (..),
    CacheEntry (..),

    -- * Supporting Types
    TypeKind (..),
    Constructor (..),
    RecordField (..),
    Method (..),
    QueryMetadata (..),

    -- * Filter and Display Options
    FilterOptions (..),
    DisplayOptions (..),
    defaultFilterOptions,
    defaultDisplayOptions,

    -- * Utility Functions
    isCacheValid,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import GHC.Generics (Generic)

-- | Represents a Haskell package on Hackage with all its versions and metadata
data Package = Package
  { packageName :: Text,
    packageVersions :: [Version],
    packageSynopsis :: Text,
    packageDescription :: Text,
    packageMaintainer :: Text,
    packageHomepage :: Maybe Text,
    packageDocUrl :: Text,
    packageUploadDate :: UTCTime,
    packageDependencies :: [Dependency],
    packageModules :: [Module] -- version-specific
  }
  deriving (Show, Eq, Generic)

instance ToJSON Package

instance FromJSON Package

-- | Represents a specific version of a package with release metadata
data Version = Version
  { versionNumber :: Text, -- e.g., "2.2.3.0"
    versionReleaseDate :: UTCTime,
    versionIsPrerelease :: Bool, -- alpha, beta, RC
    versionIsLatest :: Bool,
    versionIsPreferred :: Bool -- Hackage preferred version
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Version

instance FromJSON Version

-- | Represents a Haskell module within a specific package version
data Module = Module
  { moduleName :: Text, -- qualified name, e.g., "Data.Aeson.Types"
    modulePackage :: Text, -- package name
    moduleVersion :: Text, -- version number
    moduleExportedFunctions :: [Function],
    moduleExportedTypes :: [Type],
    moduleExportedClasses :: [TypeClass],
    moduleDocumentation :: Maybe Text, -- Haddock module-level docs
    moduleSourceUrl :: Maybe Text -- link to source code
  }
  deriving (Show, Eq, Generic)

instance ToJSON Module

instance FromJSON Module

-- | Represents an exported function in a module
data Function = Function
  { functionName :: Text,
    functionSignature :: Text, -- full type signature
    functionDocumentation :: Maybe Text, -- Haddock comments
    functionSourceCode :: Maybe Text -- implementation source
  }
  deriving (Show, Eq, Generic)

instance ToJSON Function

instance FromJSON Function

-- | Kind of type definition
data TypeKind = DataType | NewType | TypeAlias | TypeFamily
  deriving (Show, Eq, Generic)

instance ToJSON TypeKind

instance FromJSON TypeKind

-- | Constructor for algebraic data types
data Constructor = Constructor
  { constructorName :: Text,
    constructorFields :: [Text] -- field types
  }
  deriving (Show, Eq, Generic)

instance ToJSON Constructor

instance FromJSON Constructor

-- | Record field definition
data RecordField = RecordField
  { recordFieldName :: Text,
    recordFieldType :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON RecordField

instance FromJSON RecordField

-- | Represents a data type or type alias in a module
data Type = Type
  { typeName :: Text,
    typeDefinition :: Text, -- full definition (data/newtype/type)
    typeConstructors :: [Constructor], -- for ADTs
    typeRecordFields :: [RecordField], -- for records
    typeDocumentation :: Maybe Text,
    typeKind :: TypeKind
  }
  deriving (Show, Eq, Generic)

instance ToJSON Type

instance FromJSON Type

-- | Method in a type class
data Method = Method
  { methodName :: Text,
    methodSignature :: Text,
    methodDefaultImpl :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Method

instance FromJSON Method

-- | Represents a type class in a module
data TypeClass = TypeClass
  { typeClassName :: Text,
    typeClassParams :: [Text], -- type parameters
    typeClassConstraints :: [Text], -- superclass constraints
    typeClassMethods :: [Method],
    typeClassDocumentation :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON TypeClass

instance FromJSON TypeClass

-- | Represents a package dependency relationship
data Dependency = Dependency
  { depPackageName :: Text,
    depVersionConstraint :: Text -- e.g., ">=1.0 && <2.0"
  }
  deriving (Show, Eq, Generic)

instance ToJSON Dependency

instance FromJSON Dependency

-- | Query metadata tracking performance and cache status
data QueryMetadata = QueryMetadata
  { queryTime :: UTCTime,
    queryDuration :: NominalDiffTime, -- execution time
    queryCacheHit :: Bool, -- whether result was cached
    queryCacheAge :: Maybe NominalDiffTime -- age of cached data
  }
  deriving (Show, Eq, Generic)

instance ToJSON QueryMetadata

instance FromJSON QueryMetadata

-- | Represents the response to a user query with metadata
data QueryResult a = QueryResult
  { queryData :: a, -- Package, [Package], Module, etc.
    queryMetadata :: QueryMetadata,
    querySuggestions :: [Text] -- alternative suggestions on partial matches
  }
  deriving (Show, Eq, Generic, Functor)

instance (ToJSON a) => ToJSON (QueryResult a)

instance (FromJSON a) => FromJSON (QueryResult a)

-- | Represents a cached query result with TTL
data CacheEntry a = CacheEntry
  { cacheData :: a,
    cacheTimestamp :: UTCTime,
    cacheTTL :: NominalDiffTime, -- seconds until expiration
    cacheKey :: Text -- unique key for this cache entry
  }
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (CacheEntry a)

instance (FromJSON a) => FromJSON (CacheEntry a)

-- | Check if cache entry is still valid
isCacheValid :: CacheEntry a -> UTCTime -> Bool
isCacheValid entry now =
  diffUTCTime now (cacheTimestamp entry) < cacheTTL entry

-- | Filter options for controlling which entity types to display
data FilterOptions = FilterOptions
  { filterFunctions :: Bool, -- Show functions?
    filterTypes :: Bool, -- Show types?
    filterClasses :: Bool -- Show classes?
  }
  deriving (Show, Eq, Generic)

instance ToJSON FilterOptions

instance FromJSON FilterOptions

-- | Default filter options (show all entity types)
defaultFilterOptions :: FilterOptions
defaultFilterOptions = FilterOptions True True True

-- | Display options for controlling output verbosity
data DisplayOptions = DisplayOptions
  { withComments :: Bool -- Include Haddock documentation?
  }
  deriving (Show, Eq, Generic)

instance ToJSON DisplayOptions

instance FromJSON DisplayOptions

-- | Default display options (concise output without comments)
defaultDisplayOptions :: DisplayOptions
defaultDisplayOptions = DisplayOptions False
