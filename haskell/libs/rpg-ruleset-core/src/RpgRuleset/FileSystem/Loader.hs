{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RpgRuleset.FileSystem.Loader
  ( loadRuleFromFile
  , loadSystemFromDirectory
  , loadAllRulesFromDirectory
  , LoadError(..)
  ) where

import Control.Monad (forM, filterM)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Yaml (decodeEither')
import System.Directory
import System.FilePath

import RpgRuleset.Core.Types
import RpgRuleset.Core.Rule
import RpgRuleset.Core.System
import RpgRuleset.Parser.Yaml
import RpgRuleset.Parser.Markdown

-- | Errors that can occur during loading
data LoadError
  = FileNotFound !FilePath
  | ParseError !FilePath !Text
  | InvalidStructure !Text
  | DuplicateRuleId !RuleId !FilePath !FilePath
  deriving (Show, Eq)

-- | Load a single rule from a markdown file
loadRuleFromFile :: SystemId -> FilePath -> IO (Either LoadError Rule)
loadRuleFromFile sysId path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ FileNotFound path
    else do
      content <- BS.readFile path
      now <- getCurrentTime
      let textContent = TE.decodeUtf8 content
      case parseRuleFrontmatter textContent of
        Left err -> return $ Left $ ParseError path (T.pack $ show err)
        Right fm -> do
          let markdownContent = extractContentAfterFrontmatter textContent
              rule = Rule
                { ruleId = fmRuleId fm
                , ruleCategory = fmCategory fm
                , ruleSystemId = maybe sysId id (fmSystemId fm)
                , ruleTitle = fmTitle fm
                , ruleContent = markdownContent
                , ruleTags = fmTags fm
                , ruleVisibility = fmVisibility fm
                , ruleVersion = fmVersion fm
                , ruleChangelog = []
                , ruleRelatedRules = fmRelatedRules fm
                , ruleCrossSystemRefs = []
                , ruleConditions = Nothing
                , ruleFormulas = Nothing
                , ruleSourceFile = path
                , ruleLoadedAt = Just now
                }
          return $ Right rule

-- | Load all rules from a directory (recursively)
loadAllRulesFromDirectory :: SystemId -> FilePath -> IO (Either LoadError [Rule])
loadAllRulesFromDirectory sysId dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return $ Left $ FileNotFound dir
    else do
      files <- findMarkdownFiles dir
      results <- forM files (loadRuleFromFile sysId)
      case sequence results of
        Left err -> return $ Left err
        Right rules -> return $ checkDuplicates rules

-- | Check for duplicate rule IDs in loaded rules
checkDuplicates :: [Rule] -> Either LoadError [Rule]
checkDuplicates rules = go rules Map.empty
  where
    go [] _ = Right rules
    go (r:rs) seen =
      let rid = ruleId r
          path = ruleSourceFile r
      in case Map.lookup rid seen of
        Just existingPath -> Left $ DuplicateRuleId rid existingPath path
        Nothing -> go rs (Map.insert rid path seen)

-- | Load a system from a directory
loadSystemFromDirectory :: FilePath -> IO (Either LoadError System)
loadSystemFromDirectory dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return $ Left $ FileNotFound dir
    else do
      let systemFile = dir </> "system.yaml"
      sysExists <- doesFileExist systemFile
      if sysExists
        then loadSystemWithYaml dir systemFile
        else inferSystemFromDirectory dir

-- | Load system from system.yaml file
loadSystemWithYaml :: FilePath -> FilePath -> IO (Either LoadError System)
loadSystemWithYaml dir yamlPath = do
  content <- BS.readFile yamlPath
  case decodeEither' content of
    Left err -> return $ Left $ ParseError yamlPath (T.pack $ show err)
    Right sys -> do
      let sysId = systemId sys
      rulesResult <- loadAllRulesFromDirectory sysId dir
      case rulesResult of
        Left err -> return $ Left err
        Right rules ->
          let rulesMap = Map.fromList [(ruleId r, r) | r <- rules]
          in return $ Right sys
              { systemRules = rulesMap
              , systemRootPath = dir
              }

-- | Infer system from directory structure when no system.yaml
inferSystemFromDirectory :: FilePath -> IO (Either LoadError System)
inferSystemFromDirectory dir = do
  let dirName = takeFileName dir
      sysId = SystemId (T.pack dirName)
  rulesResult <- loadAllRulesFromDirectory sysId dir
  case rulesResult of
    Left err -> return $ Left err
    Right rules ->
      let rulesMap = Map.fromList [(ruleId r, r) | r <- rules]
          categories = Set.toList $ Set.fromList [ruleCategory r | r <- rules]
      in return $ Right $ (mkBaseSystem sysId (T.pack dirName))
          { systemRules = rulesMap
          , systemCategories = categories
          , systemRootPath = dir
          }

-- | Find all markdown files in a directory recursively
findMarkdownFiles :: FilePath -> IO [FilePath]
findMarkdownFiles dir = do
  contents <- listDirectory dir
  let paths = map (dir </>) contents
  files <- filterM doesFileExist paths
  dirs <- filterM doesDirectoryExist paths
  let mdFiles = filter isMarkdownFile files
  subFiles <- concat <$> mapM findMarkdownFiles dirs
  return $ mdFiles ++ subFiles
  where
    isMarkdownFile f = takeExtension f `elem` [".md", ".markdown"]