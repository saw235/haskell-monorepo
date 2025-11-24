{-# LANGUAGE OverloadedStrings #-}

module CLI.Options
  ( Options (..),
    Command (..),
    QueryOptions (..),
    ValidateOptions (..),
    ListOptions (..),
    ListTarget (..),
    InfoOptions (..),
    InitOptions (..),
    OutputFormat (..),
    parseOptions,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import RpgRuleset.Core.Types (Category (..), SystemId (..), Tag (..), UserRole (..))

-- | Top-level CLI options
data Options = Options
  { optCommand :: !Command,
    optDataDir :: !FilePath,
    optRole :: !UserRole,
    optOutputFormat :: !OutputFormat,
    optVerbose :: !Bool
  }
  deriving (Show, Eq)

-- | Available CLI commands
data Command
  = QueryCmd !QueryOptions
  | ValidateCmd !ValidateOptions
  | ListCmd !ListOptions
  | InfoCmd !InfoOptions
  | InitCmd !InitOptions
  deriving (Show, Eq)

-- | Options for query command
data QueryOptions = QueryOptions
  { qoKeywords :: ![Text],
    qoCategory :: !(Maybe Category),
    qoSystem :: !(Maybe SystemId),
    qoTags :: ![Tag],
    qoLimit :: !Int,
    qoShowRelated :: !Bool
  }
  deriving (Show, Eq)

-- | Options for validate command
data ValidateOptions = ValidateOptions
  { voFilePath :: !FilePath,
    voStrict :: !Bool,
    voCheckAll :: !Bool
  }
  deriving (Show, Eq)

-- | Options for list command
data ListOptions = ListOptions
  { loTarget :: !ListTarget,
    loSystem :: !(Maybe SystemId)
  }
  deriving (Show, Eq)

-- | What to list
data ListTarget
  = ListSystems
  | ListCategories
  | ListRules
  deriving (Show, Eq)

-- | Options for info command
data InfoOptions = InfoOptions
  { ioRuleId :: !Text,
    ioShowChangelog :: !Bool
  }
  deriving (Show, Eq)

-- | Options for init command
data InitOptions = InitOptions
  { initPath :: !FilePath,
    initSystemId :: !Text,
    initSystemName :: !Text
  }
  deriving (Show, Eq)

-- | Output format
data OutputFormat
  = TextFormat
  | JsonFormat
  | MarkdownFormat
  deriving (Show, Eq)

-- | Parse CLI options
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Query and validate tabletop RPG rulesets"
            <> header "rpg-ruleset-query - A tool for managing RPG rules"
        )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> commandParser
    <*> strOption
      ( long "data-dir"
          <> short 'd'
          <> metavar "DIR"
          <> value "."
          <> help "Root directory containing rulesets"
      )
    <*> roleParser
    <*> formatParser
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Enable verbose output"
      )

roleParser :: Parser UserRole
roleParser =
  option
    (eitherReader parseRole)
    ( long "role"
        <> short 'r'
        <> metavar "ROLE"
        <> value Player
        <> help "User role: player (default) or gm"
    )
  where
    parseRole "player" = Right Player
    parseRole "gm" = Right GameMaster
    parseRole "gamemaster" = Right GameMaster
    parseRole s = Left $ "Unknown role: " ++ s ++ ". Use 'player' or 'gm'."

formatParser :: Parser OutputFormat
formatParser =
  option
    (eitherReader parseFormat)
    ( long "format"
        <> short 'f'
        <> metavar "FORMAT"
        <> value TextFormat
        <> help "Output format: text (default), json, or markdown"
    )
  where
    parseFormat "text" = Right TextFormat
    parseFormat "json" = Right JsonFormat
    parseFormat "markdown" = Right MarkdownFormat
    parseFormat "md" = Right MarkdownFormat
    parseFormat s = Left $ "Unknown format: " ++ s

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "query" (info (QueryCmd <$> queryOptionsParser) (progDesc "Search for rules"))
        <> command "validate" (info (ValidateCmd <$> validateOptionsParser) (progDesc "Validate rule files"))
        <> command "list" (info (ListCmd <$> listOptionsParser) (progDesc "List systems, categories, or rules"))
        <> command "info" (info (InfoCmd <$> infoOptionsParser) (progDesc "Show detailed rule information"))
        <> command "init" (info (InitCmd <$> initOptionsParser) (progDesc "Initialize a new ruleset"))
    )

queryOptionsParser :: Parser QueryOptions
queryOptionsParser =
  QueryOptions
    <$> many (strArgument (metavar "KEYWORDS" <> help "Search keywords"))
    <*> optional
      ( Category . T.pack
          <$> strOption
            ( long "category"
                <> short 'c'
                <> metavar "CATEGORY"
                <> help "Filter by category"
            )
      )
    <*> optional
      ( SystemId . T.pack
          <$> strOption
            ( long "system"
                <> short 's'
                <> metavar "SYSTEM"
                <> help "Filter by system"
            )
      )
    <*> many
      ( Tag . T.pack
          <$> strOption
            ( long "tag"
                <> short 't'
                <> metavar "TAG"
                <> help "Filter by tag (can be specified multiple times)"
            )
      )
    <*> option
      auto
      ( long "limit"
          <> short 'n'
          <> metavar "N"
          <> value 10
          <> help "Maximum number of results (default: 10)"
      )
    <*> switch
      ( long "show-related"
          <> help "Show related rules"
      )

validateOptionsParser :: Parser ValidateOptions
validateOptionsParser =
  ValidateOptions
    <$> strArgument (metavar "FILE" <> help "Rule file to validate")
    <*> switch
      ( long "strict"
          <> help "Treat warnings as errors"
      )
    <*> switch
      ( long "all"
          <> help "Validate all files in directory"
      )

listOptionsParser :: Parser ListOptions
listOptionsParser =
  ListOptions
    <$> listTargetParser
    <*> optional
      ( SystemId . T.pack
          <$> strOption
            ( long "system"
                <> short 's'
                <> metavar "SYSTEM"
                <> help "Filter by system"
            )
      )

listTargetParser :: Parser ListTarget
listTargetParser =
  hsubparser
    ( command "systems" (info (pure ListSystems) (progDesc "List available systems"))
        <> command "categories" (info (pure ListCategories) (progDesc "List categories"))
        <> command "rules" (info (pure ListRules) (progDesc "List all rules"))
    )

infoOptionsParser :: Parser InfoOptions
infoOptionsParser =
  InfoOptions
    <$> strArgument (metavar "RULE_ID" <> help "Rule ID to show info for")
    <*> switch
      ( long "changelog"
          <> help "Show version changelog"
      )

initOptionsParser :: Parser InitOptions
initOptionsParser =
  InitOptions
    <$> strArgument (metavar "PATH" <> help "Directory to initialize")
    <*> strOption
      ( long "id"
          <> metavar "ID"
          <> help "System ID (e.g., my-rpg)"
      )
    <*> strOption
      ( long "name"
          <> metavar "NAME"
          <> help "System display name"
      )
