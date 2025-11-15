{-# LANGUAGE OverloadedStrings #-}

module HackageClient.TreeDisplay
  ( displayPackageTree,
    displayVersionList,
    formatCacheStatus,
    displayModule,
    displayModuleWithOptions,
  )
where

import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Tree (Tree (..), drawTree)
import HackageClient.Filter (applyFilters)
import HackageClient.Types
  ( Module (..),
    Package (..),
    QueryMetadata (..),
    QueryResult (..),
    Version (..),
    Function (..),
    Type (..),
    TypeClass (..),
    FilterOptions (..),
    DisplayOptions (..),
    defaultFilterOptions,
    defaultDisplayOptions,
  )

-- | Display package as a tree structure (T028)
displayPackageTree :: QueryResult Package -> String
displayPackageTree qr =
  let pkg = queryData qr
      cacheStatus = formatCacheStatus (queryMetadata qr)
      tree = packageToTree pkg
   in cacheStatus ++ "\n" ++ drawTree tree

-- | Convert Package to Tree structure
packageToTree :: Package -> Tree String
packageToTree pkg =
  Node
    (T.unpack (packageName pkg) ++ " - " ++ T.unpack (packageSynopsis pkg))
    [ Node ("Documentation: " ++ T.unpack (packageDocUrl pkg)) [],
      Node ("Maintainer: " ++ T.unpack (packageMaintainer pkg)) [],
      Node
        ("Homepage: " ++ maybe "N/A" T.unpack (packageHomepage pkg))
        [],
      Node ("Versions: " ++ show (length (packageVersions pkg)) ++ " available") $
        map versionNode (take 5 $ packageVersions pkg), -- Show first 5 versions
      Node ("Modules: " ++ show (length (packageModules pkg))) $
        map moduleNode (packageModules pkg)
    ]

-- | Format a version as a tree node
versionNode :: Version -> Tree String
versionNode v =
  let vNum = T.unpack (versionNumber v)
      tags = concat [
          if versionIsLatest v then " [latest]" else "",
          if versionIsPreferred v then " [preferred]" else "",
          if versionIsPrerelease v then " [prerelease]" else ""
        ]
   in Node (vNum ++ tags) []

-- | Format a module as a tree node (basic)
moduleNode :: Module -> Tree String
moduleNode m =
  Node (T.unpack (moduleName m)) []

-- | Format cache status indicator (T035)
formatCacheStatus :: QueryMetadata -> String
formatCacheStatus meta
  | queryCacheHit meta =
      case queryCacheAge meta of
        Nothing -> "[cached: unknown age]"
        Just age -> "[cached: " ++ formatAge age ++ " ago]"
  | otherwise = "[live]"

-- | Format age in human-readable format
formatAge :: NominalDiffTime -> String
formatAge age
  | hours < 1 = show (round minutes :: Int) ++ "m"
  | hours < 24 = show (round hours :: Int) ++ "h"
  | otherwise = show (round days :: Int) ++ "d"
  where
    seconds = realToFrac age :: Double
    minutes = seconds / 60
    hours = minutes / 60
    days = hours / 24

-- | Display version list with dates and indicators (T041, T044)
displayVersionList :: QueryResult Package -> String
displayVersionList qr =
  let pkg = queryData qr
      cacheStatus = formatCacheStatus (queryMetadata qr)
      versions = packageVersions pkg
      header = cacheStatus ++ "\n" ++ T.unpack (packageName pkg) ++ " - All Versions (" ++ show (length versions) ++ " total)"
      versionLines = map formatVersionLine versions
   in unlines (header : "" : versionLines)

-- | Format a single version line with date and tags
formatVersionLine :: Version -> String
formatVersionLine v =
  let vNum = T.unpack (versionNumber v)
      -- Release date placeholder (would need real data from Hackage)
      -- For now using the placeholder date
      tags =
        concat
          [ if versionIsLatest v then " [latest]" else "",
            if versionIsPreferred v then " [preferred]" else "",
            if versionIsPrerelease v then " [prerelease]" else ""
          ]
   in "  " ++ vNum ++ tags

-- | Display module details as a tree structure (T059)
-- Uses default options (show all, no comments)
displayModule :: Module -> String
displayModule = displayModuleWithOptions defaultFilterOptions defaultDisplayOptions

-- | Display module with custom filter and display options (T077, T078)
displayModuleWithOptions :: FilterOptions -> DisplayOptions -> Module -> String
displayModuleWithOptions filterOpts displayOpts mod =
  let filteredMod = applyFilters filterOpts mod
      tree = moduleToTree displayOpts filteredMod
   in drawTree tree

-- | Convert Module to Tree structure with full details
moduleToTree :: DisplayOptions -> Module -> Tree String
moduleToTree displayOpts mod =
  Node
    (T.unpack (moduleName mod) ++ " (" ++ T.unpack (modulePackage mod) ++ "-" ++ T.unpack (moduleVersion mod) ++ ")")
    [ Node "Functions:" (map (functionTreeNode displayOpts) (moduleExportedFunctions mod)),
      Node "Types:" (map (typeTreeNode displayOpts) (moduleExportedTypes mod)),
      Node "Type Classes:" (map (typeClassTreeNode displayOpts) (moduleExportedClasses mod))
    ]

-- | Convert Function to Tree node (with optional documentation)
functionTreeNode :: DisplayOptions -> Function -> Tree String
functionTreeNode displayOpts func =
  let name = T.unpack (functionName func)
      sig = T.unpack (functionSignature func)
      docNode =
        if withComments displayOpts
          then case functionDocumentation func of
            Just doc -> [Node ("  -- " ++ T.unpack doc) []]
            Nothing -> []
          else []
   in Node (name ++ " " ++ sig) docNode

-- | Convert Type to Tree node (with optional documentation)
typeTreeNode :: DisplayOptions -> Type -> Tree String
typeTreeNode displayOpts typ =
  let name = T.unpack (typeName typ)
      docNode =
        if withComments displayOpts
          then case typeDocumentation typ of
            Just doc -> [Node ("  -- " ++ T.unpack doc) []]
            Nothing -> []
          else []
   in Node name docNode

-- | Convert TypeClass to Tree node (with optional documentation)
typeClassTreeNode :: DisplayOptions -> TypeClass -> Tree String
typeClassTreeNode displayOpts tc =
  let name = T.unpack (typeClassName tc)
      docNode =
        if withComments displayOpts
          then case typeClassDocumentation tc of
            Just doc -> [Node ("  -- " ++ T.unpack doc) []]
            Nothing -> []
          else []
   in Node name docNode
