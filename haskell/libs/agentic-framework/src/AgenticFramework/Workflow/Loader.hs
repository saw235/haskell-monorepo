{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Loader
-- Description : Load capabilities from JSON files
-- Copyright   : (c) 2025
-- License     : MIT
--
-- Load capability definitions from JSON files on disk.
-- Default location: ~/.config/agentic-framework/capabilities/
module AgenticFramework.Workflow.Loader
  ( -- * Loading Single Capabilities
    loadCapability,
    loadCapabilityFromFile,

    -- * Loading Multiple Capabilities
    loadCapabilities,
    loadCapabilitiesFromDirectory,

    -- * Loading Result Types
    LoadResult (..),
    LoadError (..),

    -- * Default Paths
    defaultCapabilitiesDir,
    getCapabilitiesDir,
  )
where

import AgenticFramework.Workflow.Capabilities (capabilityFromDef)
import AgenticFramework.Workflow.Schema
  ( CapabilitySchema (..),
    CapabilitySetSchema (..),
    schemaToCapabilityDef,
    validateCapabilitySchema,
  )
import AgenticFramework.Workflow.Types (Capability, CapabilityDef)
import Control.Exception (IOException, catch)
import Control.Monad (filterM, forM)
import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (partitionEithers)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getHomeDirectory,
    listDirectory,
  )
import System.FilePath (takeExtension, (</>))

-- | Errors that can occur during capability loading
data LoadError
  = FileNotFound FilePath
  | ParseError FilePath Text
  | ValidationError FilePath Text
  | DirectoryNotFound FilePath
  | IOError FilePath Text
  deriving (Show, Eq)

-- | Result of loading a capability
data LoadResult a
  = LoadSuccess a
  | LoadFailure LoadError
  deriving (Show, Eq)

-- | Get the default capabilities directory
--   ~/.config/agentic-framework/capabilities/
defaultCapabilitiesDir :: IO FilePath
defaultCapabilitiesDir = do
  home <- getHomeDirectory
  return $ home </> ".config" </> "agentic-framework" </> "capabilities"

-- | Get the capabilities directory from environment or default
getCapabilitiesDir :: IO FilePath
getCapabilitiesDir = defaultCapabilitiesDir

-- | Load a capability by name from the default directory
--   Looks for: <capabilitiesDir>/<name>.json
loadCapability :: Text -> IO (LoadResult Capability)
loadCapability name = do
  dir <- getCapabilitiesDir
  let filePath = dir </> T.unpack name <> ".json"
  loadCapabilityFromFile filePath

-- | Load a capability from a specific file path
loadCapabilityFromFile :: FilePath -> IO (LoadResult Capability)
loadCapabilityFromFile filePath = do
  exists <- doesFileExist filePath
  if not exists
    then return $ LoadFailure $ FileNotFound filePath
    else do
      result <- safeDecodeFile filePath
      case result of
        Left err -> return $ LoadFailure $ ParseError filePath (T.pack err)
        Right schema -> case validateCapabilitySchema schema of
          Left validationErr ->
            return $ LoadFailure $ ValidationError filePath (T.pack $ show validationErr)
          Right () ->
            let def = schemaToCapabilityDef schema
             in return $ LoadSuccess $ capabilityFromDef def

-- | Safely decode a JSON file, catching IO exceptions
safeDecodeFile :: FilePath -> IO (Either String CapabilitySchema)
safeDecodeFile filePath =
  (eitherDecodeFileStrict filePath)
    `catch` (\e -> return $ Left $ "IO error: " <> show (e :: IOException))

-- | Load all capabilities from the default directory
loadCapabilities :: IO ([LoadError], [Capability])
loadCapabilities = do
  dir <- getCapabilitiesDir
  loadCapabilitiesFromDirectory dir

-- | Load all capabilities from a specific directory
loadCapabilitiesFromDirectory :: FilePath -> IO ([LoadError], [Capability])
loadCapabilitiesFromDirectory dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return ([DirectoryNotFound dir], [])
    else do
      files <- listDirectory dir
      let jsonFiles = filter isJsonFile files
          fullPaths = map (dir </>) jsonFiles
      results <- forM fullPaths loadCapabilityFromFile
      let (failures, successes) = partitionResults results
      return (failures, successes)

-- | Check if a file is a JSON file
isJsonFile :: FilePath -> Bool
isJsonFile path = takeExtension path == ".json"

-- | Partition load results into failures and successes
partitionResults :: [LoadResult a] -> ([LoadError], [a])
partitionResults = foldr categorize ([], [])
  where
    categorize (LoadSuccess a) (errs, caps) = (errs, a : caps)
    categorize (LoadFailure e) (errs, caps) = (e : errs, caps)
