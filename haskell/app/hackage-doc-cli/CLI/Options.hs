{-# LANGUAGE OverloadedStrings #-}

module CLI.Options
  ( Options (..),
    Command (..),
    parseOptions,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

-- | CLI Commands
data Command
  = QueryPackage Text (Maybe Text) Bool (Maybe Text) -- Query a package (packageName, maybeVersion, listVersions, maybeModule)
  | QueryModule Text (Maybe Text) Text -- Query a specific module (packageName, maybeVersion, moduleName)
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

-- | Query package command with optional --version, --list-versions, and --module flags (T042, T050, T062)
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
