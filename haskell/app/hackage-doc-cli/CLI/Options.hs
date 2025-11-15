{-# LANGUAGE OverloadedStrings #-}

module CLI.Options
  ( Options (..),
    Command (..),
    FilterOpts (..),
    DisplayOpts (..),
    parseOptions,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

-- | Filter options for controlling output
data FilterOpts = FilterOpts
  { optFilterFunctions :: Bool,
    optFilterTypes :: Bool,
    optFilterClasses :: Bool
  }
  deriving (Show, Eq)

-- | Display options for controlling verbosity
data DisplayOpts = DisplayOpts
  { optWithComments :: Bool
  }
  deriving (Show, Eq)

-- | CLI Commands
data Command
  = QueryPackage Text (Maybe Text) Bool (Maybe Text) FilterOpts DisplayOpts -- Query a package (packageName, maybeVersion, listVersions, maybeModule, filterOpts, displayOpts)
  | QueryModule Text (Maybe Text) Text FilterOpts DisplayOpts -- Query a specific module (packageName, maybeVersion, moduleName, filterOpts, displayOpts)
  deriving (Show, Eq)

-- | CLI Options
data Options = Options
  { optCommand :: Command
  }
  deriving (Show, Eq)

-- | Parse command-line options (T030)
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Query Hackage documentation from the command line"
            <> header "hackage-doc-cli - Hackage documentation query tool"
        )

-- | Options parser
optionsParser :: Parser Options
optionsParser = Options <$> commandParser

-- | Command parser
commandParser :: Parser Command
commandParser = queryPackageCommand

-- | Parse filter options (T075)
filterOptsParser :: Parser FilterOpts
filterOptsParser =
  FilterOpts
    <$> switch
      ( long "filter-functions"
          <> help "Show only exported functions"
      )
    <*> switch
      ( long "filter-types"
          <> help "Show only exported types"
      )
    <*> switch
      ( long "filter-classes"
          <> help "Show only exported type classes"
      )

-- | Parse display options (T076)
displayOptsParser :: Parser DisplayOpts
displayOptsParser =
  DisplayOpts
    <$> switch
      ( long "with-comments"
          <> help "Include Haddock documentation text alongside signatures"
      )

-- | Query package command with optional flags (T042, T050, T062, T075, T076)
queryPackageCommand :: Parser Command
queryPackageCommand =
  QueryPackage
    <$> (T.pack <$> argument str (metavar "PACKAGE" <> help "Package name to query"))
    <*> optional
      ( T.pack
          <$> strOption
            ( long "version"
                <> short 'v'
                <> metavar "VERSION"
                <> help "Query specific package version (e.g., 2.2.3.0)"
            )
      )
    <*> switch
      ( long "list-versions"
          <> short 'l'
          <> help "List all available versions of the package"
      )
    <*> optional
      ( T.pack
          <$> strOption
            ( long "module"
                <> short 'm'
                <> metavar "MODULE"
                <> help "Query specific module (e.g., Data.Aeson)"
            )
      )
    <*> filterOptsParser
    <*> displayOptsParser
