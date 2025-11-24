{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Output
  ( -- * Query Results
    outputQueryResultsText
  , outputQueryResultsJson
  , outputQueryResultsMarkdown
    -- * Validation Results
  , outputValidationResultsText
  , outputValidationResultsJson
    -- * List Output
  , outputSystemsList
  , outputCategoriesList
  , outputRulesList
    -- * Rule Info
  , outputRuleInfo
  ) where

import Control.Monad (when)
import Data.Aeson (encode, Value)
import Data.Aeson.Types (ToJSON(..), object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure, exitSuccess)

import RpgRuleset.Core.Types
import RpgRuleset.Core.Rule
import RpgRuleset.Core.System
import RpgRuleset.Query.Engine
import RpgRuleset.Validation.RuleId

import CLI.Options (OutputFormat(..))

-- | Output query results as text
outputQueryResultsText :: QueryResult -> Bool -> IO ()
outputQueryResultsText QueryResult{..} showRelated = do
  TIO.putStrLn $ "Found " <> T.pack (show qrTotalCount) <> " results\n"
  mapM_ (outputScoredRuleText showRelated) qrResults
  where
    outputScoredRuleText showRel ScoredRule{..} = do
      let Rule{..} = srRule
      TIO.putStrLn $ unRuleId ruleId <> " | " <> maybe "(no title)" id ruleTitle
      TIO.putStrLn $ "  Category: " <> unCategory ruleCategory
      TIO.putStrLn $ "  Tags: " <> T.intercalate ", " (map unTag $ Set.toList ruleTags)
      TIO.putStrLn $ "  Score: " <> T.pack (show srScore)
      when showRel $ do
        TIO.putStrLn $ "  Related: " <> T.intercalate ", " (map unRuleId ruleRelatedRules)
      TIO.putStrLn ""

-- | Output query results as JSON
outputQueryResultsJson :: QueryResult -> IO ()
outputQueryResultsJson result = do
  BL.putStrLn $ encode result

-- | Output query results as Markdown
outputQueryResultsMarkdown :: QueryResult -> IO ()
outputQueryResultsMarkdown QueryResult{..} = do
  TIO.putStrLn $ "# Query Results (" <> T.pack (show qrTotalCount) <> " found)\n"
  mapM_ outputScoredRuleMd qrResults
  where
    outputScoredRuleMd ScoredRule{..} = do
      let Rule{..} = srRule
      TIO.putStrLn $ "## " <> unRuleId ruleId <> " - " <> maybe "(no title)" id ruleTitle
      TIO.putStrLn ""
      TIO.putStrLn $ "- **Category**: " <> unCategory ruleCategory
      TIO.putStrLn $ "- **Tags**: " <> T.intercalate ", " (map unTag $ Set.toList ruleTags)
      TIO.putStrLn $ "- **Score**: " <> T.pack (show srScore)
      TIO.putStrLn ""

-- | Output validation results as text
outputValidationResultsText :: Either RuleIdError () -> [RuleIdWarning] -> Bool -> IO ()
outputValidationResultsText formatResult warnings strict = do
  case formatResult of
    Left (InvalidFormat rid reason) -> do
      TIO.putStrLn $ "ERROR: Invalid rule ID format: " <> unRuleId rid
      TIO.putStrLn $ "  Reason: " <> reason
    Left (DuplicateId rid) -> do
      TIO.putStrLn $ "ERROR: Duplicate rule ID: " <> unRuleId rid
    Right () -> do
      TIO.putStrLn "Rule ID format: OK"

  mapM_ outputWarning warnings

  let warningCount = length warnings
  when (warningCount > 0) $ do
    TIO.putStrLn $ "\n" <> T.pack (show warningCount) <> " warning(s)"
    when strict $ TIO.putStrLn "(--strict mode: warnings are treated as errors)"
  where
    outputWarning (NonStandardPrefix rid used suggested) = do
      TIO.putStrLn $ "WARNING: Non-standard prefix for " <> unRuleId rid
      TIO.putStrLn $ "  Used: " <> used <> ", Suggested: " <> suggested

-- | Output validation results as JSON
outputValidationResultsJson :: Either RuleIdError () -> [RuleIdWarning] -> IO ()
outputValidationResultsJson formatResult warnings = do
  let result = object
        [ "valid" .= case formatResult of { Right () -> True; Left _ -> False }
        , "errors" .= case formatResult of
            Left (InvalidFormat rid reason) -> [object ["type" .= ("invalid_format" :: Text), "rule_id" .= rid, "reason" .= reason]]
            Left (DuplicateId rid) -> [object ["type" .= ("duplicate" :: Text), "rule_id" .= rid]]
            Right () -> [] :: [Value]
        , "warnings" .= map warningToJson warnings
        ]
  BL.putStrLn $ encode result
  where
    warningToJson (NonStandardPrefix rid used suggested) = object
      [ "type" .= ("non_standard_prefix" :: Text)
      , "rule_id" .= rid
      , "used" .= used
      , "suggested" .= suggested
      ]

-- | Output systems list
outputSystemsList :: [System] -> OutputFormat -> IO ()
outputSystemsList systems fmt = case fmt of
  TextFormat -> mapM_ outputSystemText systems
  JsonFormat -> BL.putStrLn $ encode systems
  MarkdownFormat -> do
    TIO.putStrLn "# Available Systems\n"
    mapM_ outputSystemMd systems
  where
    outputSystemText System{..} = do
      TIO.putStrLn $ unSystemId systemId <> " - " <> systemName
      maybe (return ()) (\d -> TIO.putStrLn $ "  " <> d) systemDescription
    outputSystemMd System{..} = do
      TIO.putStrLn $ "- **" <> unSystemId systemId <> "**: " <> systemName

-- | Output categories list
outputCategoriesList :: System -> OutputFormat -> IO ()
outputCategoriesList System{..} fmt = case fmt of
  TextFormat -> mapM_ (TIO.putStrLn . unCategory) systemCategories
  JsonFormat -> BL.putStrLn $ encode systemCategories
  MarkdownFormat -> do
    TIO.putStrLn "# Categories\n"
    mapM_ (\c -> TIO.putStrLn $ "- " <> unCategory c) systemCategories

-- | Output rules list
outputRulesList :: [Rule] -> OutputFormat -> IO ()
outputRulesList rules fmt = case fmt of
  TextFormat -> mapM_ outputRuleText rules
  JsonFormat -> BL.putStrLn $ encode rules
  MarkdownFormat -> do
    TIO.putStrLn "# Rules\n"
    mapM_ outputRuleMd rules
  where
    outputRuleText Rule{..} =
      TIO.putStrLn $ unRuleId ruleId <> " | " <> maybe "(no title)" id ruleTitle
    outputRuleMd Rule{..} =
      TIO.putStrLn $ "- **" <> unRuleId ruleId <> "**: " <> maybe "(no title)" id ruleTitle

-- | Output detailed rule info
outputRuleInfo :: Rule -> Bool -> OutputFormat -> IO ()
outputRuleInfo rule showChangelog fmt = case fmt of
  TextFormat -> outputRuleInfoText rule showChangelog
  JsonFormat -> BL.putStrLn $ encode rule
  MarkdownFormat -> outputRuleInfoMarkdown rule showChangelog

outputRuleInfoText :: Rule -> Bool -> IO ()
outputRuleInfoText Rule{..} showChangelog = do
  TIO.putStrLn $ "Rule ID: " <> unRuleId ruleId
  maybe (return ()) (\t -> TIO.putStrLn $ "Title: " <> t) ruleTitle
  TIO.putStrLn $ "Category: " <> unCategory ruleCategory
  TIO.putStrLn $ "System: " <> unSystemId ruleSystemId
  TIO.putStrLn $ "Tags: " <> T.intercalate ", " (map unTag $ Set.toList ruleTags)
  TIO.putStrLn $ "Visibility: " <> T.pack (show ruleVisibility)
  TIO.putStrLn $ "Version: " <> showVersion ruleVersion
  TIO.putStrLn $ "Source: " <> T.pack ruleSourceFile
  TIO.putStrLn "\n--- Content ---\n"
  TIO.putStrLn ruleContent
  when (showChangelog && not (null ruleChangelog)) $ do
    TIO.putStrLn "\n--- Changelog ---\n"
    mapM_ outputChangelogEntry ruleChangelog
  where
    outputChangelogEntry ChangelogEntry{..} = do
      TIO.putStrLn $ showVersion ceVersion <> ": " <> ceDescription

outputRuleInfoMarkdown :: Rule -> Bool -> IO ()
outputRuleInfoMarkdown Rule{..} showChangelog = do
  TIO.putStrLn $ "# " <> unRuleId ruleId <> maybe "" (" - " <>) ruleTitle
  TIO.putStrLn ""
  TIO.putStrLn $ "- **Category**: " <> unCategory ruleCategory
  TIO.putStrLn $ "- **System**: " <> unSystemId ruleSystemId
  TIO.putStrLn $ "- **Tags**: " <> T.intercalate ", " (map unTag $ Set.toList ruleTags)
  TIO.putStrLn $ "- **Visibility**: " <> T.pack (show ruleVisibility)
  TIO.putStrLn $ "- **Version**: " <> showVersion ruleVersion
  TIO.putStrLn ""
  TIO.putStrLn "## Content"
  TIO.putStrLn ""
  TIO.putStrLn ruleContent
  when (showChangelog && not (null ruleChangelog)) $ do
    TIO.putStrLn "\n## Changelog\n"
    mapM_ outputChangelogEntry ruleChangelog
  where
    outputChangelogEntry ChangelogEntry{..} =
      TIO.putStrLn $ "- **" <> showVersion ceVersion <> "**: " <> ceDescription

showVersion :: Version -> Text
showVersion Version{..} = T.pack $ show vMajor <> "." <> show vMinor <> "." <> show vPatch