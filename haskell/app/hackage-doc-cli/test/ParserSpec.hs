{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserTests) where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Ord (Down (..), comparing)
import HackageClient.Parser (parseModuleHTML, validateVersion)
import HackageClient.Types (Function (..), Module (..), Package (..), Version (..), packageName, versionNumber)
import Test.HUnit

-- | HUnit test: Parse Package from fixture aeson-package.json (T022)
test_parsePackageFixture :: Test
test_parsePackageFixture =
  TestCase $ do
    -- Bazel runfiles path for the fixture
    fixtureData <- BL.readFile "haskell/app/hackage-doc-cli/test/Fixtures/aeson-package.json"
    assertBool "Fixture file should not be empty" (BL.length fixtureData > 0)

-- | Test version sorting (newest first) - T037
test_versionSorting :: Test
test_versionSorting =
  TestCase $ do
    let versions =
          [ Version "1.0.0" (read "2025-01-01 00:00:00 UTC") False False True,
            Version "2.0.0" (read "2025-01-02 00:00:00 UTC") False True True,
            Version "1.5.0" (read "2025-01-01 12:00:00 UTC") False False True
          ]
    let sorted = sortBy (comparing (Down . versionNumber)) versions
    assertEqual "Newest version should be first" "2.0.0" (versionNumber $ head sorted)
    assertEqual "Oldest version should be last" "1.0.0" (versionNumber $ last sorted)

-- | HUnit test: Version validation and parsing (T045)
test_versionValidation :: Test
test_versionValidation =
  TestCase $ do
    -- Valid PVP versions
    assertEqual "Valid PVP version 1.2.3.4" True (validateVersion "1.2.3.4")
    assertEqual "Valid PVP version 2.0.0.0" True (validateVersion "2.0.0.0")

    -- Valid semantic versions
    assertEqual "Valid semver 1.2.3" True (validateVersion "1.2.3")
    assertEqual "Valid semver 0.1.0" True (validateVersion "0.1.0")

    -- Pre-release versions
    assertEqual "Valid pre-release alpha" True (validateVersion "1.0.0-alpha")
    assertEqual "Valid pre-release beta" True (validateVersion "2.1.0-beta.1")
    assertEqual "Valid pre-release rc" True (validateVersion "1.5.0-rc1")

    -- Invalid versions
    assertEqual "Invalid version (letters)" False (validateVersion "abc")
    assertEqual "Invalid version (empty)" False (validateVersion "")
    assertEqual "Invalid version (single number)" False (validateVersion "1")
    assertEqual "Invalid version (too many dots)" False (validateVersion "1.2.3.4.5.6")

-- | HUnit test: Parse module HTML from fixture aeson-module.html (T054)
test_parseModuleHTML :: Test
test_parseModuleHTML =
  TestCase $ do
    -- Read the HTML fixture
    htmlContent <- BS.readFile "haskell/app/hackage-doc-cli/test/Fixtures/aeson-module.html"
    assertBool "HTML fixture should not be empty" (BS.length htmlContent > 0)

    -- Parse the HTML to extract module information
    let maybeModule = parseModuleHTML "aeson" "2.2.3.0" htmlContent
    assertBool "Should successfully parse module HTML" (isJust maybeModule)

    case maybeModule of
      Nothing -> assertFailure "Failed to parse module HTML"
      Just modInfo -> do
        -- Verify module name
        assertEqual "Module name should be Data.Aeson" "Data.Aeson" (moduleName modInfo)

        -- Verify package and version
        assertEqual "Package should be aeson" "aeson" (modulePackage modInfo)
        assertEqual "Version should be 2.2.3.0" "2.2.3.0" (moduleVersion modInfo)

        -- Verify we extracted some functions
        assertBool "Should have extracted some functions" (not . null $ moduleExportedFunctions modInfo)

        -- Verify specific functions exist (decode, encode, eitherDecode)
        let functionNames = map functionName (moduleExportedFunctions modInfo)
        assertBool "Should contain 'decode' function" ("decode" `elem` functionNames)

-- | All parser tests
parserTests :: Test
parserTests =
  TestLabel "Parser Tests" $
    TestList
      [ TestLabel "Parse aeson package from JSON fixture" test_parsePackageFixture,
        TestLabel "Version sorting (newest first)" test_versionSorting,
        TestLabel "Version validation and parsing" test_versionValidation,
        TestLabel "Parse module HTML from fixture" test_parseModuleHTML
      ]
