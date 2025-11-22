{-# LANGUAGE OverloadedStrings #-}

module Workflow.CapabilitySpec (spec) where

import AgenticFramework.Workflow.Capabilities
import AgenticFramework.Workflow.Loader
import AgenticFramework.Workflow.Types (Capability (..), CapabilityDef (..))
import Data.Aeson (object, (.=), encode, decode)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Capability System" $ do
  describe "applyCapability" $ do
    it "modifies the prompt using the capability's modifier" $ do
      let cap = Capability
            { capName = "uppercase",
              capDescription = "Convert to uppercase",
              capModifier = T.toUpper,
              capParameters = Nothing
            }
      applyCapability cap "hello" `shouldBe` "HELLO"

  describe "applyCapabilities" $ do
    it "applies capabilities in order" $ do
      let cap1 = Capability "append-A" "Appends A" (<> "A") Nothing
          cap2 = Capability "append-B" "Appends B" (<> "B") Nothing
      
      -- Should be (prompt <> "A") <> "B"
      applyCapabilities [cap1, cap2] "start" `shouldBe` "startAB"

    it "returns original prompt if list is empty" $ do
      applyCapabilities [] "hello" `shouldBe` "hello"

    it "is associative (property)" $ property $ \s -> do
      let cap1 = Capability "c1" "d1" (<> "1") Nothing
          cap2 = Capability "c2" "d2" (<> "2") Nothing
          cap3 = Capability "c3" "d3" (<> "3") Nothing

      applyCapabilities [cap1, cap2, cap3] (T.pack s)
        `shouldBe` applyCapabilities [cap3] (applyCapabilities [cap1, cap2] (T.pack s))

  -- User Story 3: Step-level capability application tests
  describe "[T032][US3] Step-level capability application" $ do
    it "applies capability to a single workflow step" $ pending

    it "applies different capabilities to different steps" $ pending

    it "allows capability override at step level" $ pending

  describe "[T033][US3] Capability isolation between steps" $ do
    it "step capabilities do not affect other steps" $ pending

    it "global capabilities apply to all steps" $ pending

    it "step capability overrides global capability" $ pending

  -- User Story 5: Dynamic Capability Loading tests
  describe "[T049][US5] JSON capability loading" $ do
    it "parses valid capability definition from JSON" $ do
      let validJson = object
            [ "name" .= ("reasoning" :: Text)
            , "description" .= ("Break down complex problems step-by-step" :: Text)
            , "parameters" .= object ["style" .= ("chain-of-thought" :: Text)]
            ]
      let parsed = decode (encode validJson) :: Maybe CapabilityDef
      parsed `shouldSatisfy` isJust
      case parsed of
        Just def -> do
          capDefName def `shouldBe` "reasoning"
          capDefDescription def `shouldBe` "Break down complex problems step-by-step"
        Nothing -> expectationFailure "Should have parsed valid JSON"

    it "loads capability with all required fields" $ do
      let json = object
            [ "name" .= ("code-review" :: Text)
            , "description" .= ("Reviews code for issues" :: Text)
            ]
      let parsed = decode (encode json) :: Maybe CapabilityDef
      parsed `shouldSatisfy` isJust

    it "returns capability with correct modifier based on definition" $ do
      let def = CapabilityDef
            { capDefName = "prefix-test"
            , capDefDescription = "Adds prefix to prompt"
            , capDefParameters = Nothing
            , capDefModifierType = Just "prefix"
            , capDefModifierValue = Just "[PREFIX] "
            }
      let cap = capabilityFromDef def
      applyCapability cap "hello" `shouldBe` "[PREFIX] hello"

  describe "[T050][US5] Invalid capability handling" $ do
    it "rejects JSON missing required 'name' field" $ do
      let invalidJson = object
            [ "description" .= ("A description" :: Text)
            ]
      let parsed = decode (encode invalidJson) :: Maybe CapabilityDef
      parsed `shouldSatisfy` isNothing

    it "rejects JSON missing required 'description' field" $ do
      let invalidJson = object
            [ "name" .= ("test-cap" :: Text)
            ]
      let parsed = decode (encode invalidJson) :: Maybe CapabilityDef
      parsed `shouldSatisfy` isNothing

    it "handles malformed JSON gracefully" $ do
      let malformedJson = "{not valid json"
      let parsed = decode (LBS.pack $ map (fromIntegral . fromEnum) malformedJson) :: Maybe CapabilityDef
      parsed `shouldSatisfy` isNothing

    it "handles empty JSON object" $ do
      let emptyJson = object []
      let parsed = decode (encode emptyJson) :: Maybe CapabilityDef
      parsed `shouldSatisfy` isNothing

    it "handles null values in required fields" $ do
      let nullJson = "{\"name\": null, \"description\": \"test\"}"
      let parsed = decode (LBS.pack $ map (fromIntegral . fromEnum) nullJson) :: Maybe CapabilityDef
      parsed `shouldSatisfy` isNothing

  -- Tests for loading actual JSON files from examples/capabilities/
  -- These tests use Bazel runfiles to locate the data files
  describe "[US5] Loading capabilities from actual JSON files" $ do
    it "loads reasoning.json successfully" $ do
      -- Use Bazel runfiles path
      let capPath = "haskell/libs/agentic-framework/examples/capabilities/reasoning.json"
      result <- loadCapabilityFromFile capPath
      case result of
        LoadSuccess cap -> do
          capName cap `shouldBe` "reasoning"
          capDescription cap `shouldSatisfy` (not . T.null)
        LoadFailure (FileNotFound _) -> pendingWith "JSON file not available in test sandbox"
        LoadFailure err -> expectationFailure $ "Failed to load: " ++ show err

    it "loads code-review.json successfully" $ do
      let capPath = "haskell/libs/agentic-framework/examples/capabilities/code-review.json"
      result <- loadCapabilityFromFile capPath
      case result of
        LoadSuccess cap -> capName cap `shouldBe` "code-review"
        LoadFailure (FileNotFound _) -> pendingWith "JSON file not available in test sandbox"
        LoadFailure err -> expectationFailure $ "Failed to load: " ++ show err

    it "loads concise.json successfully" $ do
      let capPath = "haskell/libs/agentic-framework/examples/capabilities/concise.json"
      result <- loadCapabilityFromFile capPath
      case result of
        LoadSuccess cap -> capName cap `shouldBe` "concise"
        LoadFailure (FileNotFound _) -> pendingWith "JSON file not available in test sandbox"
        LoadFailure err -> expectationFailure $ "Failed to load: " ++ show err

    it "loads technical.json successfully" $ do
      let capPath = "haskell/libs/agentic-framework/examples/capabilities/technical.json"
      result <- loadCapabilityFromFile capPath
      case result of
        LoadSuccess cap -> capName cap `shouldBe` "technical"
        LoadFailure (FileNotFound _) -> pendingWith "JSON file not available in test sandbox"
        LoadFailure err -> expectationFailure $ "Failed to load: " ++ show err

    it "loads friendly.json successfully" $ do
      let capPath = "haskell/libs/agentic-framework/examples/capabilities/friendly.json"
      result <- loadCapabilityFromFile capPath
      case result of
        LoadSuccess cap -> capName cap `shouldBe` "friendly"
        LoadFailure (FileNotFound _) -> pendingWith "JSON file not available in test sandbox"
        LoadFailure err -> expectationFailure $ "Failed to load: " ++ show err

    it "loads all capabilities from directory" $ do
      let capDir = "haskell/libs/agentic-framework/examples/capabilities"
      (errors, capabilities) <- loadCapabilitiesFromDirectory capDir
      case errors of
        [DirectoryNotFound _] -> pendingWith "Directory not available in test sandbox"
        [] -> length capabilities `shouldBe` 5
        _ -> expectationFailure $ "Unexpected errors: " ++ show errors

    it "applies loaded reasoning capability correctly" $ do
      let capPath = "haskell/libs/agentic-framework/examples/capabilities/reasoning.json"
      result <- loadCapabilityFromFile capPath
      case result of
        LoadSuccess cap -> do
          let modified = capModifier cap "What is 2+2?"
          -- Should have prefix applied
          modified `shouldSatisfy` (\t -> T.isPrefixOf "Let me think" t)
        LoadFailure (FileNotFound _) -> pendingWith "JSON file not available in test sandbox"
        LoadFailure err -> expectationFailure $ "Failed to load: " ++ show err

    it "applies loaded concise capability correctly" $ do
      let capPath = "haskell/libs/agentic-framework/examples/capabilities/concise.json"
      result <- loadCapabilityFromFile capPath
      case result of
        LoadSuccess cap -> do
          let modified = capModifier cap "Explain quantum physics"
          -- Should have suffix applied
          modified `shouldSatisfy` (\t -> T.isSuffixOf "elaboration." t)
        LoadFailure (FileNotFound _) -> pendingWith "JSON file not available in test sandbox"
        LoadFailure err -> expectationFailure $ "Failed to load: " ++ show err
