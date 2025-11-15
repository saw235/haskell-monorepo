{-# LANGUAGE OverloadedStrings #-}

module TreeDisplaySpec (treeDisplayTests) where

import Data.Text (Text)
import qualified Data.Text as T
import HackageClient.Types (Module (..), Function (..), Type (..), TypeClass (..))
import HackageClient.TreeDisplay (displayModule)
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

-- | All tree display tests
treeDisplayTests :: Test
treeDisplayTests =
  TestLabel "TreeDisplay Tests" $
    TestList
      [ TestLabel "Module detail tree formatting" test_moduleTreeDisplay,
        TestLabel "Empty module tree formatting" test_emptyModuleTreeDisplay
      ]
