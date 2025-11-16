{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Context.Tokenizer
-- Description : FFI bindings to Rust tokenizers for accurate token counting
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides Foreign Function Interface (FFI) bindings to the
-- tiktoken-rs Rust library for accurate model-specific token counting.
--
-- Token counting is critical for context window management, as it determines
-- when summarization should be triggered (at 90% threshold by default).
--
-- = Accuracy Requirement
--
-- Token counting must be accurate within 5% of actual model counts (SC-011).
-- This is achieved by using the reference implementation (tiktoken) rather than
-- heuristic approaches.
--
-- = Supported Models
--
-- * GPT-4 family (cl100k_base encoding)
-- * GPT-3.5-turbo (cl100k_base encoding)
-- * Claude models (cl100k_base encoding)
-- * Custom encodings via model name
--
-- = Performance
--
-- Token counting overhead target: <100ms per invocation
module AgenticFramework.Context.Tokenizer
  ( -- * Token Counting
    countTokens,
    ModelName,
    TokenCount,

    -- * Error Handling
    TokenizerError (..),
  )
where

import Control.Exception (Exception, catch, throwIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import Data.Typeable (Typeable)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Model name for tokenizer selection.
--   Examples: "gpt-4", "gpt-3.5-turbo", "claude-3-opus"
type ModelName = Text

-- | Number of tokens.
type TokenCount = Int

-- | Errors that can occur during tokenization.
data TokenizerError
  = -- | Model not supported by tokenizer
    UnsupportedModel ModelName
  | -- | General tokenization failure
    TokenizationFailed Text
  | -- | FFI call failed
    FFIError Text
  deriving (Show, Eq, Typeable)

instance Exception TokenizerError

--------------------------------------------------------------------------------
-- FFI Declarations
--------------------------------------------------------------------------------

-- | FFI binding to Rust tokenizer_count_tokens function
--
-- Returns error code (0 = success, non-zero = error)
foreign import ccall unsafe "tokenizer_count_tokens"
  c_tokenizer_count_tokens ::
    -- | model name
    CString ->
    -- | text to tokenize
    CString ->
    -- | output token count
    Ptr CInt ->
    -- | error code
    IO CInt

-- | FFI binding to check if model is supported
foreign import ccall unsafe "tokenizer_is_model_supported"
  c_tokenizer_is_model_supported :: CString -> IO CInt

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Count tokens in text for a specific model.
--
-- This function uses model-specific tokenization to accurately count
-- the number of tokens that would be consumed by the given text.
--
-- __Examples:__
--
-- @
-- -- Count tokens for GPT-4
-- count <- countTokens "gpt-4" "Hello, world!"
-- -- count should be 4 (approximately)
--
-- -- Count tokens for Claude
-- count <- countTokens "claude-3-opus" "This is a longer text..."
-- @
--
-- __Performance:__ <100ms for typical inputs (<10k characters)
--
-- __Accuracy:__ Within 5% of actual model token count
countTokens :: ModelName -> Text -> IO TokenCount
countTokens modelName text =
  withCString (T.unpack modelName) $ \cModelName ->
    withCString (T.unpack text) $ \cText -> do
      outCountPtr <- malloc :: IO (Ptr CInt)

      errorCode <- c_tokenizer_count_tokens cModelName cText outCountPtr

      case errorCode of
        0 -> do
          -- Success
          count <- peek outCountPtr
          free outCountPtr
          return $ fromIntegral count
        1 -> do
          -- Invalid model
          free outCountPtr
          throwIO $ UnsupportedModel modelName
        2 -> do
          -- Invalid UTF-8
          free outCountPtr
          throwIO $ FFIError "Invalid UTF-8 in input text"
        3 -> do
          -- Encoding error
          free outCountPtr
          throwIO $ TokenizationFailed "Failed to encode text"
        4 -> do
          -- Null pointer
          free outCountPtr
          throwIO $ FFIError "Null pointer passed to FFI"
        _ -> do
          -- Unknown error
          free outCountPtr
          throwIO $ FFIError $ T.pack $ "Unknown error code: " ++ show errorCode

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Check if a model is supported by the tokenizer.
--
-- This function queries the Rust FFI to check if tiktoken-rs
-- supports the given model name.
isSupportedModel :: ModelName -> Bool
isSupportedModel model = unsafePerformIO $
  withCString (T.unpack model) $ \cModel -> do
    result <- c_tokenizer_is_model_supported cModel
    return $ result == 1

-- | Utility to check if we're within 5% accuracy threshold.
--
-- Used in property tests to verify token counting accuracy.
isWithinAccuracyThreshold :: TokenCount -> TokenCount -> Bool
isWithinAccuracyThreshold actual estimated =
  let actualF = fromIntegral actual :: Double
      estimatedF = fromIntegral estimated :: Double
      percentDiff = abs (estimatedF - actualF) / actualF * 100.0
   in percentDiff <= 5.0
