{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RpgRuleset.Parser.Yaml
  ( parseRuleFrontmatter,
    parseFrontmatterFromFile,
    RuleFrontmatter (..),
    ParseError (..),
  )
where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (ParseException, decodeEither')
import GHC.Generics (Generic)
import RpgRuleset.Core.Types

-- | Parse errors that can occur during frontmatter parsing
data ParseError
  = YamlParseError !Text
  | MissingFrontmatter
  | InvalidFrontmatterDelimiter
  | MissingRequiredField !Text
  deriving (Show, Eq)

-- | Frontmatter extracted from a rule markdown file
data RuleFrontmatter = RuleFrontmatter
  { fmRuleId :: !RuleId,
    fmCategory :: !Category,
    fmTitle :: !(Maybe Text),
    fmTags :: !(Set Tag),
    fmVisibility :: !Visibility,
    fmVersion :: !Version,
    fmRelatedRules :: ![RuleId],
    fmSystemId :: !(Maybe SystemId)
  }
  deriving (Show, Eq)

-- | Intermediate type for JSON/YAML parsing
data RawFrontmatter = RawFrontmatter
  { rawRuleId :: !Text,
    rawCategory :: !Text,
    rawTitle :: !(Maybe Text),
    rawTags :: !(Maybe [Text]),
    rawVisibility :: !(Maybe Text),
    rawVersion :: !(Maybe Text),
    rawRelatedRules :: !(Maybe [Text]),
    rawSystemId :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON RawFrontmatter where
  parseJSON = withObject "RawFrontmatter" $ \o -> do
    rawRuleId <- o .: "rule_id"
    rawCategory <- o .: "category"
    rawTitle <- o .:? "title"
    rawTags <- o .:? "tags"
    rawVisibility <- o .:? "visibility"
    rawVersion <- o .:? "version"
    rawRelatedRules <- o .:? "related_rules"
    rawSystemId <- o .:? "system_id"
    return RawFrontmatter {..}

-- | Parse YAML frontmatter from text
parseRuleFrontmatter :: Text -> Either ParseError RuleFrontmatter
parseRuleFrontmatter input = do
  yamlContent <- extractFrontmatter input
  raw <- parseYaml yamlContent
  convertRawFrontmatter raw

-- | Extract YAML content from between --- delimiters
extractFrontmatter :: Text -> Either ParseError Text
extractFrontmatter input =
  let ls = T.lines input
   in case ls of
        ("---" : rest) ->
          case break (== "---") rest of
            (yamlLines, "---" : _) -> Right (T.unlines yamlLines)
            _ -> Left InvalidFrontmatterDelimiter
        _ -> Left MissingFrontmatter

-- | Parse YAML text into RawFrontmatter
parseYaml :: Text -> Either ParseError RawFrontmatter
parseYaml yamlText =
  case decodeEither' (TE.encodeUtf8 yamlText) of
    Left err -> Left $ YamlParseError (T.pack $ show err)
    Right raw -> Right raw

-- | Convert raw frontmatter to typed frontmatter
convertRawFrontmatter :: RawFrontmatter -> Either ParseError RuleFrontmatter
convertRawFrontmatter RawFrontmatter {..} =
  Right
    RuleFrontmatter
      { fmRuleId = RuleId rawRuleId,
        fmCategory = Category rawCategory,
        fmTitle = rawTitle,
        fmTags = Set.fromList $ map Tag (maybe [] id rawTags),
        fmVisibility = parseVisibility rawVisibility,
        fmVersion = parseVersion rawVersion,
        fmRelatedRules = map RuleId (maybe [] id rawRelatedRules),
        fmSystemId = SystemId <$> rawSystemId
      }

-- | Parse visibility from text
parseVisibility :: Maybe Text -> Visibility
parseVisibility (Just "gm-only") = GMOnly
parseVisibility (Just "gm_only") = GMOnly
parseVisibility (Just "gmonly") = GMOnly
parseVisibility _ = Public

-- | Parse version from text (e.g., "1.0.0")
parseVersion :: Maybe Text -> Version
parseVersion Nothing = Version 1 0 0
parseVersion (Just v) =
  case map (read . T.unpack) $ T.splitOn "." v of
    [major, minor, patch] -> Version major minor patch
    [major, minor] -> Version major minor 0
    [major] -> Version major 0 0
    _ -> Version 1 0 0

-- | Parse frontmatter from a file path
parseFrontmatterFromFile :: FilePath -> IO (Either ParseError RuleFrontmatter)
parseFrontmatterFromFile path = do
  content <- BS.readFile path
  return $ parseRuleFrontmatter (TE.decodeUtf8 content)
