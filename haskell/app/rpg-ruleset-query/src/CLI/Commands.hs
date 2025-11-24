{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Commands
  ( runCommand,
    runQuery,
    runValidate,
    runList,
    runInfo,
    runInit,
  )
where

import CLI.Options
import CLI.Output
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import RpgRuleset.Core.Rule
import RpgRuleset.Core.System hiding (getAllRules)
import RpgRuleset.Core.Types
import RpgRuleset.FileSystem.Loader
import RpgRuleset.FileSystem.Structure
import RpgRuleset.Query.Engine
import RpgRuleset.Query.Index
import RpgRuleset.Validation.RuleId
import System.Exit (exitFailure, exitSuccess)

-- | Run the appropriate command based on options
runCommand :: Options -> IO ()
runCommand opts = do
  case optCommand opts of
    QueryCmd qopts -> runQuery opts qopts
    ValidateCmd vopts -> runValidate opts vopts
    ListCmd lopts -> runList opts lopts
    InfoCmd iopts -> runInfo opts iopts
    InitCmd initOpts -> runInit opts initOpts

-- | Execute query command
runQuery :: Options -> QueryOptions -> IO ()
runQuery opts QueryOptions {..} = do
  -- Load system(s)
  loadResult <- loadSystemFromDirectory (optDataDir opts)
  case loadResult of
    Left err -> do
      TIO.putStrLn $ "Error loading system: " <> T.pack (show err)
      exitFailure
    Right sys -> do
      -- Build index
      let idx = buildIndex [sys]

      -- Build query
      let query =
            Query
              { qKeywords = qoKeywords,
                qFilterCategory = qoCategory,
                qFilterSystem = qoSystem,
                qFilterTags = qoTags,
                qFilterVisibility = Just (roleToVisibility $ optRole opts),
                qLimit = qoLimit,
                qOffset = 0
              }

      -- Execute query
      let results = executeQuery query idx

      -- Output results
      case optOutputFormat opts of
        TextFormat -> outputQueryResultsText results qoShowRelated
        JsonFormat -> outputQueryResultsJson results
        MarkdownFormat -> outputQueryResultsMarkdown results

-- | Execute validate command
runValidate :: Options -> ValidateOptions -> IO ()
runValidate opts ValidateOptions {..} = do
  -- Load the rule file
  loadResult <- loadRuleFromFile (SystemId "validation") voFilePath
  case loadResult of
    Left err -> do
      TIO.putStrLn $ "Error loading file: " <> T.pack (show err)
      exitFailure
    Right rule -> do
      -- Validate rule ID format
      let formatResult = checkRuleIdFormat (ruleId rule)
          prefixWarnings = checkPrefixConventions (ruleId rule) (ruleCategory rule)

      -- Output validation results
      case optOutputFormat opts of
        TextFormat -> outputValidationResultsText formatResult prefixWarnings voStrict
        JsonFormat -> outputValidationResultsJson formatResult prefixWarnings
        MarkdownFormat -> outputValidationResultsText formatResult prefixWarnings voStrict

      -- Exit with appropriate code
      case (formatResult, voStrict, null prefixWarnings) of
        (Left _, _, _) -> exitFailure
        (Right (), True, False) -> exitFailure
        _ -> exitSuccess

-- | Execute list command
runList :: Options -> ListOptions -> IO ()
runList opts ListOptions {..} = do
  loadResult <- loadSystemFromDirectory (optDataDir opts)
  case loadResult of
    Left err -> do
      TIO.putStrLn $ "Error loading system: " <> T.pack (show err)
      exitFailure
    Right sys -> do
      let idx = buildIndex [sys]
      case loTarget of
        ListSystems -> outputSystemsList [sys] (optOutputFormat opts)
        ListCategories -> outputCategoriesList sys (optOutputFormat opts)
        ListRules -> do
          let rules = case loSystem of
                Nothing -> getAllRules idx
                Just sid -> getRulesBySystem sid idx
              visibleRules = filter (isVisibleTo $ optRole opts) rules
          outputRulesList visibleRules (optOutputFormat opts)

-- | Execute info command
runInfo :: Options -> InfoOptions -> IO ()
runInfo opts InfoOptions {..} = do
  loadResult <- loadSystemFromDirectory (optDataDir opts)
  case loadResult of
    Left err -> do
      TIO.putStrLn $ "Error loading system: " <> T.pack (show err)
      exitFailure
    Right sys -> do
      let idx = buildIndex [sys]
          rid = RuleId ioRuleId
      case lookupRuleById rid idx of
        Nothing -> do
          TIO.putStrLn $ "Rule not found: " <> ioRuleId
          exitFailure
        Just rule -> do
          when (not $ isVisibleTo (optRole opts) rule) $ do
            TIO.putStrLn "This rule is not visible to your role."
            exitFailure
          outputRuleInfo rule ioShowChangelog (optOutputFormat opts)

-- | Execute init command
runInit :: Options -> InitOptions -> IO ()
runInit _ InitOptions {..} = do
  createSystemStructure initPath (SystemId initSystemId) initSystemName
  TIO.putStrLn $ "Initialized new ruleset at: " <> T.pack initPath
  TIO.putStrLn "Created:"
  TIO.putStrLn "  - system.yaml"
  TIO.putStrLn "  - README.md"
  TIO.putStrLn "  - character-creation/"
  TIO.putStrLn "  - world-building/"
  TIO.putStrLn "  - interactions/"

-- | Convert user role to visibility filter
roleToVisibility :: UserRole -> Visibility
roleToVisibility Player = Public
roleToVisibility GameMaster = GMOnly -- GM can see all, but we filter to GMOnly to include both
