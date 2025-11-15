{-# LANGUAGE OverloadedStrings #-}

module TreeDisplaySpec (treeDisplayTests) where

import Data.Text (Text)
import qualified Data.Text as T
import HackageClient.Types
  ( Module (..),
    Function (..),
    Type (..),
    TypeClass (..),
    FilterOptions (..),
    DisplayOptions (..),
    TypeKind (..),
  )
import HackageClient.TreeDisplay (displayModule, displayModuleWithOptions)
import Test.HUnit

-- | Test module detail tree formatting (T055)
test_moduleTreeDisplay :: Test
test_moduleTreeDisplay =
  TestCase $ do
    let testModule = Module
          { moduleName = "Data.Aeson",
            modulePackage = "aeson",
            moduleVersion = "2.2.3.0",
            moduleExportedFunctions =
              [ Function
                  { functionName = "decode",
                    functionSignature = ":: FromJSON a => ByteString -> Maybe a",
                    functionDocumentation = Just "Parse a JSON value from a ByteString.",
                    functionSourceCode = Nothing
                  },
                Function
                  { functionName = "encode",
                    functionSignature = ":: ToJSON a => a -> ByteString",
                    functionDocumentation = Just "Encode a Haskell value as a JSON ByteString.",
                    functionSourceCode = Nothing
                  }
              ],
            moduleExportedTypes = [],
            moduleExportedClasses = [],
            moduleDocumentation = Just "Types and functions for working efficiently with JSON data.",
            moduleSourceUrl = Just "https://hackage.haskell.org/package/aeson-2.2.3.0/docs/src/Data.Aeson.html"
          }

    let treeDisplay = displayModule testModule

    -- Verify the tree contains module name
    assertBool "Tree should contain module name" (T.isInfixOf "Data.Aeson" (T.pack treeDisplay))

    -- Verify the tree contains function names
    assertBool "Tree should contain 'decode' function" (T.isInfixOf "decode" (T.pack treeDisplay))
    assertBool "Tree should contain 'encode' function" (T.isInfixOf "encode" (T.pack treeDisplay))

    -- Verify the tree contains function signatures
    assertBool "Tree should contain function signature" (T.isInfixOf "::" (T.pack treeDisplay))

-- | Test tree display with empty module
test_emptyModuleTreeDisplay :: Test
test_emptyModuleTreeDisplay =
  TestCase $ do
    let emptyModule = Module
          { moduleName = "Data.Empty",
            modulePackage = "test",
            moduleVersion = "1.0.0",
            moduleExportedFunctions = [],
            moduleExportedTypes = [],
            moduleExportedClasses = [],
            moduleDocumentation = Nothing,
            moduleSourceUrl = Nothing
          }

    let treeDisplay = displayModule emptyModule

    -- Verify the tree contains module name even if empty
    assertBool "Tree should contain module name" (T.isInfixOf "Data.Empty" (T.pack treeDisplay))

-- | Helper function to create a test module with mixed entity types
createMixedModule :: Module
createMixedModule =
  Module
    { moduleName = "Data.Mixed",
      modulePackage = "test-pkg",
      moduleVersion = "1.0.0",
      moduleExportedFunctions =
        [ Function
            { functionName = "testFunc",
              functionSignature = ":: Int -> String",
              functionDocumentation = Just "Test function documentation",
              functionSourceCode = Nothing
            }
        ],
      moduleExportedTypes =
        [ Type
            { typeName = "TestType",
              typeDefinition = "data TestType = TestValue",
              typeConstructors = [],
              typeRecordFields = [],
              typeDocumentation = Just "Test type documentation",
              typeKind = DataType
            }
        ],
      moduleExportedClasses =
        [ TypeClass
            { typeClassName = "TestClass",
              typeClassParams = ["a"],
              typeClassConstraints = [],
              typeClassMethods = [],
              typeClassDocumentation = Just "Test class documentation"
            }
        ],
      moduleDocumentation = Just "Test module",
      moduleSourceUrl = Nothing
    }

-- | Test --filter-functions flag (T066)
test_filterFunctions :: Test
test_filterFunctions =
  TestCase $ do
    let filterOpts = FilterOptions True False False
        displayOpts = DisplayOptions False
        testMod = createMixedModule
        result = displayModuleWithOptions filterOpts displayOpts testMod

    -- Should contain functions
    assertBool "Should contain function name" (T.isInfixOf "testFunc" (T.pack result))
    -- Should NOT contain types or classes
    assertBool "Should not contain type name" (not $ T.isInfixOf "TestType" (T.pack result))
    assertBool "Should not contain class name" (not $ T.isInfixOf "TestClass" (T.pack result))

-- | Test --filter-types flag (T067)
test_filterTypes :: Test
test_filterTypes =
  TestCase $ do
    let filterOpts = FilterOptions False True False
        displayOpts = DisplayOptions False
        testMod = createMixedModule
        result = displayModuleWithOptions filterOpts displayOpts testMod

    -- Should contain types
    assertBool "Should contain type name" (T.isInfixOf "TestType" (T.pack result))
    -- Should NOT contain functions or classes
    assertBool "Should not contain function name" (not $ T.isInfixOf "testFunc" (T.pack result))
    assertBool "Should not contain class name" (not $ T.isInfixOf "TestClass" (T.pack result))

-- | Test --filter-classes flag (T068)
test_filterClasses :: Test
test_filterClasses =
  TestCase $ do
    let filterOpts = FilterOptions False False True
        displayOpts = DisplayOptions False
        testMod = createMixedModule
        result = displayModuleWithOptions filterOpts displayOpts testMod

    -- Should contain classes
    assertBool "Should contain class name" (T.isInfixOf "TestClass" (T.pack result))
    -- Should NOT contain functions or types
    assertBool "Should not contain function name" (not $ T.isInfixOf "testFunc" (T.pack result))
    assertBool "Should not contain type name" (not $ T.isInfixOf "TestType" (T.pack result))

-- | Test combined filter flags (T069)
test_combinedFilters :: Test
test_combinedFilters =
  TestCase $ do
    let filterOpts = FilterOptions True True False -- functions and types only
        displayOpts = DisplayOptions False
        testMod = createMixedModule
        result = displayModuleWithOptions filterOpts displayOpts testMod

    -- Should contain functions and types
    assertBool "Should contain function name" (T.isInfixOf "testFunc" (T.pack result))
    assertBool "Should contain type name" (T.isInfixOf "TestType" (T.pack result))
    -- Should NOT contain classes
    assertBool "Should not contain class name" (not $ T.isInfixOf "TestClass" (T.pack result))

-- | Test --with-comments flag showing documentation (T070)
test_withComments :: Test
test_withComments =
  TestCase $ do
    let filterOpts = FilterOptions True False False
        displayOpts = DisplayOptions True -- Enable comments
        testMod = createMixedModule
        result = displayModuleWithOptions filterOpts displayOpts testMod

    -- Should contain function documentation
    assertBool "Should contain function documentation" (T.isInfixOf "Test function documentation" (T.pack result))

-- | Test default behavior (no --with-comments) showing only signatures (T071)
test_withoutComments :: Test
test_withoutComments =
  TestCase $ do
    let filterOpts = FilterOptions True False False
        displayOpts = DisplayOptions False -- Disable comments (default)
        testMod = createMixedModule
        result = displayModuleWithOptions filterOpts displayOpts testMod

    -- Should contain function name and signature
    assertBool "Should contain function name" (T.isInfixOf "testFunc" (T.pack result))
    assertBool "Should contain function signature" (T.isInfixOf ":: Int -> String" (T.pack result))
    -- Should NOT contain documentation
    assertBool "Should not contain function documentation" (not $ T.isInfixOf "Test function documentation" (T.pack result))

-- | All tree display tests
treeDisplayTests :: Test
treeDisplayTests =
  TestLabel "TreeDisplay Tests" $
    TestList
      [ TestLabel "Module detail tree formatting" test_moduleTreeDisplay,
        TestLabel "Empty module tree formatting" test_emptyModuleTreeDisplay,
        TestLabel "Filter functions only (T066)" test_filterFunctions,
        TestLabel "Filter types only (T067)" test_filterTypes,
        TestLabel "Filter classes only (T068)" test_filterClasses,
        TestLabel "Combined filters (T069)" test_combinedFilters,
        TestLabel "With comments flag (T070)" test_withComments,
        TestLabel "Without comments flag (T071)" test_withoutComments
      ]
