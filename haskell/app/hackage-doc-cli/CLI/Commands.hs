{-# LANGUAGE OverloadedStrings #-}

module CLI.Commands
  ( handleCommand,
  )
where

import CLI.Options (Command (..), FilterOpts (..), DisplayOpts (..))
import Control.Exception (catch, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import HackageClient (QueryResult (..), queryPackageWithCache)
import HackageClient.API (fetchModuleDetails)
import HackageClient.Parser (validateVersion, parseModuleHTML)
import HackageClient.TreeDisplay (displayPackageTree, displayVersionList, displayModule, displayModuleWithOptions)
import HackageClient.Types (Package (..), Version (..), FilterOptions (..), DisplayOptions (..))
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

-- | Handle CLI command execution (T031, T043, T051, T063, T079)
handleCommand :: Command -> IO ()
handleCommand (QueryPackage pkgName maybeVersion listVersions maybeModule filterOpts displayOpts) = do
  -- Check if module query is requested
  case maybeModule of
    Just moduleName -> handleModuleQuery pkgName maybeVersion moduleName filterOpts displayOpts
    Nothing -> handlePackageQuery pkgName maybeVersion listVersions

handleCommand (QueryModule pkgName maybeVersion moduleName filterOpts displayOpts) =
  handleModuleQuery pkgName maybeVersion moduleName filterOpts displayOpts

-- | Handle package query (without module)
handlePackageQuery :: Text -> Maybe Text -> Bool -> IO ()
handlePackageQuery pkgName maybeVersion listVersions = do
  -- Validate version if provided (T052)
  case maybeVersion of
    Just version ->
      if not (validateVersion version)
        then handleInvalidVersion pkgName version
        else runQuery
    Nothing -> runQuery
  where
    runQuery = do
      result <- queryPackageWithCache pkgName maybeVersion `catch` networkErrorHandler
      case result of
        Left err -> handlePackageNotFound pkgName err -- T034
        Right queryResult ->
          if listVersions
            then do
              -- List versions mode (T043)
              putStrLn $ displayVersionList queryResult
              exitWith ExitSuccess
            else do
              -- Normal package tree mode
              putStrLn $ displayPackageTree queryResult
              exitWith ExitSuccess

-- | Convert CLI filter options to library FilterOptions
toFilterOptions :: FilterOpts -> FilterOptions
toFilterOptions opts =
  -- If no filters are specified (all False), show all (all True)
  -- Otherwise, show only what's explicitly requested
  let hasAnyFilter = optFilterFunctions opts || optFilterTypes opts || optFilterClasses opts
   in if hasAnyFilter
        then FilterOptions (optFilterFunctions opts) (optFilterTypes opts) (optFilterClasses opts)
        else FilterOptions True True True -- Default: show all

-- | Convert CLI display options to library DisplayOptions
toDisplayOptions :: DisplayOpts -> DisplayOptions
toDisplayOptions opts = DisplayOptions (optWithComments opts)

-- | Handle module query (T063, T079)
handleModuleQuery :: Text -> Maybe Text -> Text -> FilterOpts -> DisplayOpts -> IO ()
handleModuleQuery pkgName maybeVersion moduleName filterOpts displayOpts = do
  -- Validate version if provided
  case maybeVersion of
    Just v ->
      if not (validateVersion v)
        then handleInvalidVersion pkgName v
        else runModuleQuery v
    Nothing -> do
      -- No version specified - fetch latest from package info
      packageResult <- queryPackageWithCache pkgName Nothing `catch` networkErrorHandler
      case packageResult of
        Left err -> handlePackageNotFound pkgName err
        Right queryResult -> do
          let pkg = queryData queryResult
          case packageVersions pkg of
            [] -> do
              hPutStrLn stderr $ "Error: No versions found for package " ++ T.unpack pkgName
              exitWith (ExitFailure 1)
            (latestVer:_) -> runModuleQuery (versionNumber latestVer)
  where
    runModuleQuery version = do
      -- Fetch module details from Hackage
      result <- fetchModuleDetails pkgName version moduleName `catch` networkErrorHandler
      case result of
        Left err -> handleModuleNotFound pkgName moduleName err -- T064
        Right htmlContent -> do
          -- Parse the HTML to extract module information
          case parseModuleHTML pkgName version htmlContent of
            Nothing -> do
              hPutStrLn stderr $ "Error: Failed to parse module documentation for " ++ T.unpack moduleName
              exitWith (ExitFailure 1)
            Just modInfo -> do
              -- Convert CLI options to library options
              let filterOptions = toFilterOptions filterOpts
                  displayOptions = toDisplayOptions displayOpts
              -- Display the module tree with options
              putStrLn $ displayModuleWithOptions filterOptions displayOptions modInfo
              exitWith ExitSuccess

-- | Handle network failures with clear messages (T033)
networkErrorHandler :: SomeException -> IO (Either String a)
networkErrorHandler e = do
  hPutStrLn stderr $ "Network error: " ++ show e
  hPutStrLn stderr "Please check your internet connection and try again."
  exitWith (ExitFailure 2) -- Exit code 2 for network errors

-- | Handle package not found with suggestions (T034)
handlePackageNotFound :: Text -> String -> IO ()
handlePackageNotFound pkgName err = do
  hPutStrLn stderr $ "Error: " ++ err
  hPutStrLn stderr $ "Package '" ++ T.unpack pkgName ++ "' not found on Hackage."
  hPutStrLn stderr ""
  hPutStrLn stderr "Suggestions:"
  hPutStrLn stderr "  - Check the spelling of the package name"
  hPutStrLn stderr "  - Search on https://hackage.haskell.org/packages/search"
  hPutStrLn stderr "  - Use 'hdoc --search-module MODULE' to find which package contains a module"
  exitWith (ExitFailure 1) -- Exit code 1 for package not found

-- | Handle invalid version format with suggestions (T052)
handleInvalidVersion :: Text -> Text -> IO ()
handleInvalidVersion pkgName version = do
  hPutStrLn stderr $ "Error: Invalid version format '" ++ T.unpack version ++ "'"
  hPutStrLn stderr ""
  hPutStrLn stderr "Version must follow:"
  hPutStrLn stderr "  - Semantic versioning (e.g., 1.2.3, 2.0.0)"
  hPutStrLn stderr "  - Package Versioning Policy/PVP (e.g., 1.2.3.4, 2.2.3.0)"
  hPutStrLn stderr "  - Pre-release tags (e.g., 1.0.0-alpha, 2.1.0-beta.1, 1.5.0-rc1)"
  hPutStrLn stderr ""
  hPutStrLn stderr $ "To see available versions for '" ++ T.unpack pkgName ++ "':"
  hPutStrLn stderr $ "  hdoc " ++ T.unpack pkgName ++ " --list-versions"
  exitWith (ExitFailure 3) -- Exit code 3 for invalid arguments

-- | Handle module not found with suggestions (T064)
handleModuleNotFound :: Text -> Text -> String -> IO ()
handleModuleNotFound pkgName moduleName err = do
  hPutStrLn stderr $ "Error: " ++ err
  hPutStrLn stderr $ "Module '" ++ T.unpack moduleName ++ "' not found in package '" ++ T.unpack pkgName ++ "'."
  hPutStrLn stderr ""
  hPutStrLn stderr "Suggestions:"
  hPutStrLn stderr "  - Check the spelling of the module name (case-sensitive)"
  hPutStrLn stderr "  - Ensure the module is exported by the package"
  hPutStrLn stderr $ "  - View package contents: hdoc " ++ T.unpack pkgName
  hPutStrLn stderr "  - Check module documentation on Hackage"
  exitWith (ExitFailure 1) -- Exit code 1 for module not found
