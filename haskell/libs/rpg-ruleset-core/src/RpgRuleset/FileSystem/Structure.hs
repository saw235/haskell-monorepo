{-# LANGUAGE OverloadedStrings #-}

module RpgRuleset.FileSystem.Structure
  ( -- * Directory Structure
    createSystemStructure,
    getExpectedStructure,
    validateStructure,
    StructureError (..),

    -- * Path Conventions
    getRulesDirectory,
    getCategoryDirectory,
    getSystemYamlPath,
  )
where

import Control.Monad (forM_, unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import RpgRuleset.Core.Types
import System.Directory
import System.FilePath

-- | Errors related to directory structure
data StructureError
  = MissingDirectory !FilePath
  | MissingSystemYaml !FilePath
  | InvalidCategoryDirectory !FilePath !Text
  deriving (Show, Eq)

-- | Expected structure for a system directory
data ExpectedStructure = ExpectedStructure
  { esRootDir :: !FilePath,
    esSystemYaml :: !FilePath,
    esCategoryDirs :: ![FilePath],
    esReadme :: !FilePath
  }
  deriving (Show, Eq)

-- | Get expected structure for a system
getExpectedStructure :: FilePath -> ExpectedStructure
getExpectedStructure root =
  ExpectedStructure
    { esRootDir = root,
      esSystemYaml = root </> "system.yaml",
      esCategoryDirs =
        [ root </> "character-creation",
          root </> "world-building",
          root </> "interactions"
        ],
      esReadme = root </> "README.md"
    }

-- | Validate that a directory has expected structure
validateStructure :: FilePath -> IO [StructureError]
validateStructure root = do
  let expected = getExpectedStructure root
  errors <-
    sequence
      [ checkDir (esRootDir expected),
        checkFile (esSystemYaml expected)
      ]
  catErrors <- mapM checkDir (esCategoryDirs expected)
  return $ concat (errors ++ catErrors)
  where
    checkDir path = do
      exists <- doesDirectoryExist path
      return $ if exists then [] else [MissingDirectory path]
    checkFile path = do
      exists <- doesFileExist path
      return $ if exists then [] else [MissingSystemYaml path]

-- | Create a new system directory structure
createSystemStructure :: FilePath -> SystemId -> Text -> IO ()
createSystemStructure root (SystemId sysIdText) name = do
  -- Create root directory
  createDirectoryIfMissing True root

  -- Create category directories
  forM_ defaultCategories $ \(Category cat) -> do
    let catDir = root </> T.unpack cat
    createDirectoryIfMissing True catDir
    -- Create placeholder README in each category
    TIO.writeFile (catDir </> "README.md") $
      "# " <> cat <> "\n\nAdd rules for " <> cat <> " here.\n"

  -- Create system.yaml
  TIO.writeFile (root </> "system.yaml") $
    T.unlines
      [ "system_id: " <> sysIdText,
        "name: " <> name,
        "type: base",
        "version: 1.0.0",
        "categories:",
        "  - character-creation",
        "  - world-building",
        "  - interactions"
      ]

  -- Create README.md
  TIO.writeFile (root </> "README.md") $
    T.unlines
      [ "# " <> name,
        "",
        "A ruleset for tabletop RPG games.",
        "",
        "## Structure",
        "",
        "- `character-creation/` - Rules for creating characters",
        "- `world-building/` - Rules for building game worlds",
        "- `interactions/` - Rules for social and combat interactions",
        "",
        "## Usage",
        "",
        "```bash",
        "rpg-ruleset-query query --system " <> sysIdText <> " \"your search terms\"",
        "```"
      ]
  where
    defaultCategories =
      [ Category "character-creation",
        Category "world-building",
        Category "interactions"
      ]

-- | Get rules directory for a system
getRulesDirectory :: FilePath -> FilePath
getRulesDirectory = id -- Rules are directly in system directory

-- | Get directory for a specific category
getCategoryDirectory :: FilePath -> Category -> FilePath
getCategoryDirectory root (Category cat) = root </> T.unpack cat

-- | Get path to system.yaml
getSystemYamlPath :: FilePath -> FilePath
getSystemYamlPath root = root </> "system.yaml"
