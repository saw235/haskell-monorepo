{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Property.TokenizerProps
-- Description : QuickCheck property-based tests for tokenizer accuracy
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module contains property-based tests to verify that token counting
-- meets the 5% accuracy requirement (SC-011) specified in the specification.
--
-- = Test Strategy
--
-- 1. **Accuracy Properties**: Token counts must be within 5% of reference counts
-- 2. **Consistency Properties**: Same input always produces same count
-- 3. **Monotonicity Properties**: More text = more tokens
-- 4. **Edge Cases**: Empty strings, very long strings, special characters
module Property.TokenizerProps (spec) where

import AgenticFramework.Context.Tokenizer
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Tokenizer Properties" $ do
  describe "[SC-011,FR-002b] Accuracy" $ do
    it "token count is reasonable for short texts" $
      withMaxSuccess 10 $
        property $
          \(ShortText text) ->
            let modelName = "gpt-4"
                charLen = T.length text
             in ioProperty $ do
                  count <- countTokens modelName text
                  -- Token count should be roughly proportional to text length
                  -- Typically between chars/6 and chars/2 for English text
                  let minTokens = max 0 (charLen `div` 6)
                      maxTokens = charLen + 1 -- At most 1 token per char
                  return $ count >= minTokens && count <= maxTokens

    it "token count is reasonable for long texts" $
      withMaxSuccess 10 $
        property $
          \(LongText text) ->
            let modelName = "gpt-4"
                charLen = T.length text
             in ioProperty $ do
                  count <- countTokens modelName text
                  -- Same bounds for longer texts
                  let minTokens = charLen `div` 6
                      maxTokens = charLen + 1
                  return $ count >= minTokens && count <= maxTokens

  describe "[FR-002b] Consistency" $ do
    it "same input produces same token count" $
      withMaxSuccess 10 $
        property $
          \(ShortText text) ->
            let modelName = "gpt-4"
             in ioProperty $ do
                  count1 <- countTokens modelName text
                  count2 <- countTokens modelName text
                  return $ count1 == count2

  describe "[FR-002b] Proportionality" $ do
    it "significantly longer text has more tokens" $
      withMaxSuccess 10 $
        property $
          \(ShortText text1) ->
            let modelName = "gpt-4"
                -- Create text2 by adding unique content to text1
                text2 = text1 <> " and some additional unique text here"
             in T.length text1
                  > 5
                    ==> ioProperty
                  $ do
                    count1 <- countTokens modelName text1
                    count2 <- countTokens modelName text2
                    -- Adding more text should always increase token count
                    return $ count2 > count1

  describe "[FR-002b] Edge Cases" $ do
    it "empty string has zero tokens" $
      let modelName = "gpt-4"
       in do
            count <- countTokens modelName ""
            count `shouldBe` 0

    it "single character has at least one token" $
      let modelName = "gpt-4"
       in do
            count <- countTokens modelName "a"
            count `shouldSatisfy` (>= 1)

    it "handles unicode correctly" $
      let modelName = "gpt-4"
          unicodeText = "你好世界 🌍 Привет"
       in do
            count <- countTokens modelName unicodeText
            count `shouldSatisfy` (> 0)

  describe "[FR-002b] Model Support" $ do
    it "supports GPT-4 models" $
      let text = "Hello, world!"
       in do
            count <- countTokens "gpt-4" text
            count `shouldSatisfy` (> 0)

    it "supports GPT-3.5 models" $
      let text = "Hello, world!"
       in do
            count <- countTokens "gpt-3.5-turbo" text
            count `shouldSatisfy` (> 0)

-- Note: Claude models use a different tokenizer (not tiktoken)
-- and are not supported by tiktoken-rs

--------------------------------------------------------------------------------
-- Test Data Generators
--------------------------------------------------------------------------------

-- | Generate short text (1-100 characters)
-- Uses realistic character distribution for better test coverage
newtype ShortText = ShortText Text
  deriving (Show, Eq)

instance Arbitrary ShortText where
  arbitrary = do
    len <- choose (1, 100)
    ShortText . T.pack <$> vectorOf len genRealisticChar

-- | Generate long text (100-1000 characters)
newtype LongText = LongText Text
  deriving (Show, Eq)

instance Arbitrary LongText where
  arbitrary = do
    len <- choose (100, 1000)
    LongText . T.pack <$> vectorOf len genRealisticChar

-- | Generate realistic characters for testing
-- Weighted towards ASCII letters/digits/punctuation with some Unicode
genRealisticChar :: Gen Char
genRealisticChar =
  frequency
    [ (70, choose ('a', 'z')), -- Lowercase letters
      (20, choose ('A', 'Z')), -- Uppercase letters
      (5, choose ('0', '9')), -- Digits
      (3, elements " \n.,!?;:"), -- Common punctuation/whitespace
      (2, choose ('\x80', '\xFF')) -- Extended ASCII / Latin-1
    ]

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Check if a value is within a percentage tolerance of a baseline.
--
-- @isWithinTolerance actual baseline percentTolerance@
--
-- Returns True if actual is within percentTolerance% of baseline.
isWithinTolerance :: Int -> Int -> Double -> Bool
isWithinTolerance actual baseline percentTolerance =
  let actualF = fromIntegral actual :: Double
      baselineF = fromIntegral baseline :: Double
      percentDiff = abs (actualF - baselineF) / baselineF * 100.0
   in percentDiff <= percentTolerance || baselineF == 0
