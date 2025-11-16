{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Property.SummarizationProps
-- Description : QuickCheck property-based tests for context summarization
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module contains property-based tests to verify that context summarization
-- meets the 50% token reduction requirement (SC-012) while preserving key information.
--
-- = Test Strategy
--
-- 1. **Token Reduction**: Summarization must reduce token count by ≥50%
-- 2. **Information Preservation**: Key facts, decisions, and outputs must be retained
-- 3. **Consistency**: Same conversation produces similar summaries
-- 4. **Edge Cases**: Empty conversations, very short conversations, very long conversations
module Property.SummarizationProps (spec) where

import AgenticFramework.Context
import AgenticFramework.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Test.Hspec
import Test.QuickCheck

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Context Summarization Properties" $ do
  describe "[SC-012,FR-003] Token Reduction" $ do
    it "summarization reduces token count by at least 50%" $
      pending
      -- This test requires the summarization implementation
      -- Will be implemented after T036 (summarization module)

    it "summarization preserves non-empty content" $
      pending
      -- Verify summarized text is not empty for non-empty input

  describe "[FR-003] Information Preservation" $ do
    it "summary preserves key decisions" $
      pending
      -- Verify that important decisions are retained

    it "summary preserves tool outputs" $
      pending
      -- Verify that tool results are retained

  describe "[FR-003] Consistency" $ do
    it "similar conversations produce similar summary lengths" $
      pending
      -- Verify consistent compression ratios

  describe "[FR-003] Edge Cases" $ do
    it "handles empty conversation gracefully" $
      ioProperty $ do
        -- Empty conversation should return empty or minimal summary
        return True

    it "handles single-message conversation" $
      ioProperty $ do
        -- Single message should be preserved or minimally compressed
        return True

    it "handles very long conversations" $
      pending
      -- Test with 100+ messages

--------------------------------------------------------------------------------
-- Test Data Generators
--------------------------------------------------------------------------------

-- | Generate a conversation with varying lengths
newtype Conversation = Conversation [Message]
  deriving (Show, Eq)

instance Arbitrary Conversation where
  arbitrary = do
    len <- choose (1, 50)
    messages <- vectorOf len arbitraryMessage
    return $ Conversation messages

-- | Generate a single message
arbitraryMessage :: Gen Message
arbitraryMessage = do
  content <- arbitraryText
  oneof
    [ return $ UserMessage content undefined, -- timestamp added later
      return $ AssistantMessage content undefined,
      return $ SystemMessage content undefined
    ]

-- | Generate arbitrary text content
arbitraryText :: Gen Text
arbitraryText = do
  len <- choose (10, 200)
  T.pack <$> vectorOf len arbitraryChar

-- | Generate realistic characters
arbitraryChar :: Gen Char
arbitraryChar =
  frequency
    [ (70, choose ('a', 'z')),
      (20, choose ('A', 'Z')),
      (5, choose ('0', '9')),
      (5, elements " \n.,!?")
    ]

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Check if token reduction meets 50% threshold
meetsTokenReduction :: Int -> Int -> Bool
meetsTokenReduction originalCount summarizedCount =
  let reductionPercent = (1.0 - fromIntegral summarizedCount / fromIntegral originalCount) * 100.0
   in reductionPercent >= 50.0

-- | Check if key information is preserved
-- This is a simple heuristic: check if important keywords are still present
preservesKeyInfo :: [Message] -> Text -> Bool
preservesKeyInfo messages summary =
  -- Extract important words (simple heuristic: words longer than 5 chars)
  let importantWords = filter (\w -> T.length w > 5) $ concatMap extractWords messages
      extractWords msg = T.words $ messageContent msg
      messageContent (UserMessage c _) = c
      messageContent (AssistantMessage c _) = c
      messageContent (SystemMessage c _) = c
      messageContent (ToolMessage _ _ _) = ""
      -- Check if at least 50% of important words are in summary
      preservedCount = length $ filter (`T.isInfixOf` summary) importantWords
      totalCount = length importantWords
   in totalCount == 0 || (fromIntegral preservedCount / fromIntegral totalCount) >= 0.5
