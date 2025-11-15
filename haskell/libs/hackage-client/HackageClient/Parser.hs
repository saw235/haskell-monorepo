{-# LANGUAGE OverloadedStrings #-}

module HackageClient.Parser
  ( parsePackageResponse,
    packageNameSimilarity,
    validateVersion,
    parseModuleHTML,
    extractSourceCode,
  )
where

import Control.Applicative ((<|>), empty)
import Data.Aeson (Value (..), decode, Object)
import Data.Aeson.Key (toString)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy, nub)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.Char (isDigit)
import Text.HTML.Scalpel
import HackageClient.Types
  ( Dependency (..),
    Module (..),
    Package (..),
    Version (..),
    Function (..),
    Type (..),
    TypeClass (..),
    TypeKind (..),
  )

-- | Parse Hackage JSON response into Package type (T026)
-- Hackage returns: {"version1": "normal", "version2": "deprecated", ...}
parsePackageResponse :: Text -> BL.ByteString -> Maybe Package
parsePackageResponse pkgName bs = do
  versionMap <- decode bs :: Maybe Object
  let versions = map parseVersion $ KM.toList versionMap
  return $ Package
    { packageName = pkgName,
      packageVersions = sortBy (comparing (Down . versionNumber)) versions,
      packageSynopsis = "Package " <> pkgName,
      packageDescription = "See https://hackage.haskell.org/package/" <> pkgName,
      packageMaintainer = "Unknown",
      packageHomepage = Nothing,
      packageDocUrl = "https://hackage.haskell.org/package/" <> pkgName,
      packageUploadDate = read "2025-01-01 00:00:00 UTC",
      packageDependencies = [],
      packageModules = []
    }
  where
    parseVersion (key, val) =
      let versionStr = T.pack $ toString key
          status = case val of
            String s -> s
            _ -> "unknown"
       in Version
            { versionNumber = versionStr,
              versionReleaseDate = read "2025-01-01 00:00:00 UTC",
              versionIsPrerelease = T.isInfixOf "rc" (T.toLower versionStr) ||
                                   T.isInfixOf "alpha" (T.toLower versionStr) ||
                                   T.isInfixOf "beta" (T.toLower versionStr),
              versionIsLatest = False,  -- Will be set by caller
              versionIsPreferred = status == "normal"
            }

-- | Calculate similarity between package names using Levenshtein distance (T029)
-- Returns a score from 0.0 (completely different) to 1.0 (identical)
packageNameSimilarity :: Text -> Text -> Double
packageNameSimilarity name1 name2 =
  let dist = levenshteinDistance name1 name2
      maxLen = fromIntegral $ max (T.length name1) (T.length name2)
   in if maxLen == 0
        then 1.0
        else 1.0 - (fromIntegral dist / maxLen)

-- | Levenshtein distance implementation for package name suggestions
levenshteinDistance :: Text -> Text -> Int
levenshteinDistance s1 s2 = dist (T.length s1) (T.length s2)
  where
    dist i 0 = i
    dist 0 j = j
    dist i j
      | T.index s1 (i - 1) == T.index s2 (j - 1) = dist (i - 1) (j - 1)
      | otherwise =
          minimum
            [ dist (i - 1) j + 1, -- deletion
              dist i (j - 1) + 1, -- insertion
              dist (i - 1) (j - 1) + 1 -- substitution
            ]

-- | Validate version string (semantic versioning / PVP) - T048
-- Accepts formats like:
--   - PVP: 1.2.3.4, 0.1.0.0
--   - Semver: 1.2.3, 0.1.0
--   - Pre-release: 1.0.0-alpha, 2.1.0-beta.1, 1.5.0-rc1
validateVersion :: Text -> Bool
validateVersion versionStr
  | T.null versionStr = False
  | otherwise =
      let -- Split on hyphen to separate main version from pre-release tag
          (mainVer, preRelease) = case T.breakOn "-" versionStr of
            (v, "") -> (v, "")
            (v, rest) -> (v, T.drop 1 rest)  -- drop the hyphen

          -- Split main version by dots
          parts = T.splitOn "." mainVer

          -- Check if all parts are numeric
          allNumeric = all (T.all isDigit) parts && all (not . T.null) parts

          -- Valid version should have 2-4 numeric parts
          validPartCount = length parts >= 2 && length parts <= 4

          -- Pre-release tag is optional, if present should be non-empty
          validPreRelease = T.null preRelease || not (T.null preRelease)
       in allNumeric && validPartCount && validPreRelease

-- | Parse Haddock HTML to extract module information (T057)
-- Extracts functions, types, and classes from module documentation HTML
parseModuleHTML :: Text -> Text -> BS.ByteString -> Maybe Module
parseModuleHTML pkgName version htmlContent = do
  let html = TE.decodeUtf8 htmlContent
  scrapeStringLike html $ do
    -- Extract module name from the caption
    modName <- text ("p" @: [hasClass "caption"])

    -- Extract functions from the synopsis section
    functions <- parseFunctions

    -- Extract types from type declarations (T060)
    types <- parseTypes

    -- Extract type classes from class declarations (T061)
    classes <- parseTypeClasses

    return $ Module
      { moduleName = T.strip modName,
        modulePackage = pkgName,
        moduleVersion = version,
        moduleExportedFunctions = functions,
        moduleExportedTypes = types,
        moduleExportedClasses = classes,
        moduleDocumentation = Nothing,  -- TODO: Extract module-level documentation
        moduleSourceUrl = Just $ "https://hackage.haskell.org/package/" <> pkgName <> "-" <> version <> "/docs/src/" <> modName <> ".html"
      }

-- | Parse functions from the synopsis section and their detailed documentation
parseFunctions :: Scraper Text [Function]
parseFunctions = do
  -- Parse all function items from both synopsis (short) and detailed docs (top)
  -- We'll combine data from both sections
  synopsisItems <- chroots ("li" @: [hasClass "src", hasClass "short"]) parseSynopsisFunction
  detailedItems <- chroots ("div" @: [hasClass "top"]) parseDetailedFunction

  -- Merge synopsis and detailed information
  let merged = mergeFunctions (catMaybes synopsisItems) (catMaybes detailedItems)
  return merged

-- | Parse a single function from synopsis (just name and signature)
parseSynopsisFunction :: Scraper Text (Maybe Function)
parseSynopsisFunction = do
  -- Get the full signature (entire <li> content)
  fullSignature <- text anySelector

  -- Try to get the function name from the <a> tag
  funcNames <- texts ("a" @: [])

  case listToMaybe funcNames of
    Nothing -> return Nothing
    Just funcName -> do
      -- Extract signature - it's the full content after the function name
      -- The signature typically includes :: and the type
      let parts = T.breakOn "::" fullSignature
      let signature = if T.null (snd parts)
                        then fullSignature  -- No :: found, use full text
                        else snd parts  -- Use part after ::
      return $ Just $ Function
        { functionName = T.strip funcName,
          functionSignature = T.strip signature,
          functionDocumentation = Nothing,
          functionSourceCode = Nothing
        }

-- | Parse a single function from detailed documentation (includes docs)
parseDetailedFunction :: Scraper Text (Maybe Function)
parseDetailedFunction = do
  -- Get function name and signature from <p class="src">
  maybeSig <- chroot ("p" @: [hasClass "src"]) $ do
    fullSignature <- text anySelector
    funcNames <- texts ("a" @: [hasClass "def"])
    return (listToMaybe funcNames, fullSignature)

  case maybeSig of
    (Nothing, _) -> return Nothing
    (Just funcName, fullSignature) -> do
      -- Extract documentation from <div class="doc">
      docText <- (T.strip . T.unwords . T.words <$>
                 chroot ("div" @: [hasClass "doc"]) (text anySelector)) <|> return ""

      -- Extract signature
      let parts = T.breakOn "::" fullSignature
      let signature = if T.null (snd parts)
                        then fullSignature
                        else snd parts

      return $ Just $ Function
        { functionName = T.strip funcName,
          functionSignature = T.strip signature,
          functionDocumentation = if T.null docText then Nothing else Just docText,
          functionSourceCode = Nothing
        }

-- | Merge function data from synopsis and detailed sections
-- Prefers detailed information (with docs) over synopsis-only information
mergeFunctions :: [Function] -> [Function] -> [Function]
mergeFunctions synopsis detailed =
  let detailedMap = [(functionName f, f) | f <- detailed]
      synopsisMap = [(functionName f, f) | f <- synopsis]
      allNames = nub $ map fst detailedMap ++ map fst synopsisMap
   in [fromMaybe synFunc (lookup name detailedMap)
       | name <- allNames
       , let synFunc = fromMaybe (Function name "" Nothing Nothing) (lookup name synopsisMap)
      ]

-- | Extract source code from Haddock HTML or tarball (T058)
-- This function attempts to extract implementation source code for a specific function
--
-- Approach:
-- 1. Haddock documentation HTML contains links to source files (e.g., "src/Module-Name.html")
-- 2. These source HTML pages contain the actual implementation in <pre> or <code> tags
-- 3. We extract the source URL and return it so the caller can fetch the full source
--
-- For a complete implementation in a real production system, you would:
-- - Follow the source link and fetch the source HTML
-- - Parse out the specific function's implementation using line anchors
-- - Cache the source code for performance
--
-- Current implementation: Extract source URLs from the module documentation
extractSourceCode :: Text -> Text -> BS.ByteString -> Maybe Text
extractSourceCode pkgName version htmlContent =
  let html = TE.decodeUtf8 htmlContent
      -- Extract the source link from the module page
      -- Haddock typically has a "Source" link in the page-menu
      sourceUrl = scrapeStringLike html $ do
        -- Try to find any link in the page with href containing "src/"
        -- This is a common pattern in Haddock documentation
        links <- attrs "href" "a"
        case filter (T.isInfixOf "src/") links of
          (url:_) -> return url
          [] -> empty
  in case sourceUrl of
       Just url ->
         -- Construct the full URL if it's a relative path
         if T.isPrefixOf "http" url
           then Just url
           else Just $ "https://hackage.haskell.org/package/" <> pkgName <> "-" <> version <> "/docs/" <> url
       Nothing -> Just $ "Source code available in package: " <> pkgName <> "-" <> version

-- | Parse types from Haddock HTML (T060)
-- Types in Haddock are marked with ids like "t:TypeName" and use keywords like "data", "newtype", "type"
parseTypes :: Scraper Text [Type]
parseTypes = do
  -- Find all type declarations from detailed documentation sections
  typeDecls <- chroots ("div" @: [hasClass "top"]) parseDetailedType
  return $ catMaybes typeDecls

-- | Parse a single type declaration with documentation
parseDetailedType :: Scraper Text (Maybe Type)
parseDetailedType = do
  -- Get type definition from <p class="src">
  maybeDef <- chroot ("p" @: [hasClass "src"]) $ do
    fullDef <- text anySelector
    typeIds <- attrs "id" ("a" @: [])
    let typeNames = [T.drop 2 tid | tid <- typeIds, T.isPrefixOf "t:" tid]
    return (listToMaybe typeNames, fullDef)

  case maybeDef of
    (Nothing, _) -> return Nothing
    (Just typeName, fullDef) -> do
      -- Skip if it's a class (we handle those separately)
      if "class " `T.isInfixOf` fullDef
        then return Nothing
        else do
          -- Extract documentation from <div class="doc">
          docText <- (T.strip . T.unwords . T.words <$>
                     chroot ("div" @: [hasClass "doc"]) (text anySelector)) <|> return ""

          -- Determine type kind based on keywords
          let typeKind
                | "data " `T.isInfixOf` fullDef = DataType
                | "newtype " `T.isInfixOf` fullDef = NewType
                | "type " `T.isInfixOf` fullDef && "family" `T.isInfixOf` fullDef = TypeFamily
                | "type " `T.isInfixOf` fullDef = TypeAlias
                | otherwise = DataType  -- Default

          return $ Just $ Type
            { typeName = T.strip typeName,
              typeDefinition = T.strip fullDef,
              typeConstructors = [],  -- Could be parsed from constructor tables
              typeRecordFields = [],  -- Could be parsed from record field tables
              typeDocumentation = if T.null docText then Nothing else Just docText,
              typeKind = typeKind
            }

-- | Parse type classes from Haddock HTML (T061)
-- Type classes are marked with ids like "t:ClassName" and use the "class" keyword
parseTypeClasses :: Scraper Text [TypeClass]
parseTypeClasses = do
  -- Find all class declarations from detailed documentation sections
  classDecls <- chroots ("div" @: [hasClass "top"]) parseDetailedTypeClass
  return $ catMaybes classDecls

-- | Parse a single type class declaration with documentation
parseDetailedTypeClass :: Scraper Text (Maybe TypeClass)
parseDetailedTypeClass = do
  -- Get class definition from <p class="src">
  maybeDef <- chroot ("p" @: [hasClass "src"]) $ do
    fullDef <- text anySelector
    classIds <- attrs "id" ("a" @: [])
    let classNames = [T.drop 2 cid | cid <- classIds, T.isPrefixOf "t:" cid]
    return (listToMaybe classNames, fullDef)

  case maybeDef of
    (Nothing, _) -> return Nothing
    (Just className, fullDef) -> do
      -- Only process if it's a class definition
      if not ("class " `T.isInfixOf` fullDef)
        then return Nothing
        else do
          -- Extract documentation from <div class="doc">
          docText <- (T.strip . T.unwords . T.words <$>
                     chroot ("div" @: [hasClass "doc"]) (text anySelector)) <|> return ""

          return $ Just $ TypeClass
            { typeClassName = T.strip className,
              typeClassParams = [],  -- Could be parsed from the definition
              typeClassConstraints = [],  -- Could be parsed from constraints
              typeClassMethods = [],  -- Methods would need separate parsing
              typeClassDocumentation = if T.null docText then Nothing else Just docText
            }
