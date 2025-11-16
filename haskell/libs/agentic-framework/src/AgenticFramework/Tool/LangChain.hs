{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : AgenticFramework.Tool.LangChain
Description : Wrappers for langchain-hs tools
Copyright   : (c) 2025
License     : MIT

This module provides wrappers around langchain-hs tools for use with agents.

Note: These are placeholder implementations. Full langchain-hs integration
will be implemented when the langchain-hs library provides these tools.
-}

module AgenticFramework.Tool.LangChain
  ( calculatorTool
  , wikipediaTool
  , webScraperTool
  ) where

import AgenticFramework.Types
import Data.Text (Text)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Control.Exception (try, SomeException)

--------------------------------------------------------------------------------
-- Calculator Tool
--------------------------------------------------------------------------------

-- | Calculator tool for mathematical operations.
--   Input: {"expression": "2 + 2 * 3"}
--   Output: {"result": 8, "expression": "2 + 2 * 3"}
--
--   TODO: Integrate with langchain-hs calculator when available
calculatorTool :: Tool
calculatorTool = Tool
  { toolName = "calculator"
  , toolDescription = "Perform mathematical calculations. Input: {\"expression\": \"math expression\"}. Supports +, -, *, / operations."
  , toolSchema = ToolSchema
      { inputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Mathematical expression to evaluate" :: Text)
                  ]
              ]
          , "required" .= (["expression"] :: [Text])
          ]
      , outputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "result" .= object ["type" .= ("number" :: Text)]
              , "expression" .= object ["type" .= ("string" :: Text)]
              ]
          ]
      }
  , toolExecute = \(ToolInput input) -> do
      case extractExpression input of
        Nothing -> return $ Left $ ToolExecutionError "Missing or invalid 'expression' field"
        Just expr -> do
          -- Simple eval for basic arithmetic (stub implementation)
          -- TODO: Use langchain-hs calculator or proper expression parser
          result <- try $ evaluateSimpleExpression expr
          case result of
            Left (err :: SomeException) ->
              return $ Left $ ToolExecutionError $ T.pack $ "Failed to evaluate expression: " ++ show err
            Right value ->
              return $ Right $ ToolOutput $ object
                [ "result" .= value
                , "expression" .= expr
                ]
  , toolTimeout = Just 3_000_000  -- 3 seconds
  , toolRetryable = True
  }

--------------------------------------------------------------------------------
-- Wikipedia Tool
--------------------------------------------------------------------------------

-- | Wikipedia search and retrieval tool.
--   Input: {"query": "Haskell programming language"}
--   Output: {"summary": "Haskell is a...", "url": "https://en.wikipedia.org/wiki/Haskell"}
--
--   TODO: Integrate with langchain-hs Wikipedia tool when available
wikipediaTool :: Tool
wikipediaTool = Tool
  { toolName = "wikipedia"
  , toolDescription = "Search Wikipedia and get article summaries. Input: {\"query\": \"search term\"}. Returns summary and URL."
  , toolSchema = ToolSchema
      { inputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "query" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Search query for Wikipedia" :: Text)
                  ]
              ]
          , "required" .= (["query"] :: [Text])
          ]
      , outputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "summary" .= object ["type" .= ("string" :: Text)]
              , "url" .= object ["type" .= ("string" :: Text)]
              ]
          ]
      }
  , toolExecute = \(ToolInput input) -> do
      case extractQuery input of
        Nothing -> return $ Left $ ToolExecutionError "Missing or invalid 'query' field"
        Just query -> do
          -- Stub implementation
          -- TODO: Implement actual Wikipedia API call via langchain-hs
          return $ Right $ ToolOutput $ object
            [ "summary" .= ("Wikipedia integration pending. Query was: " <> query :: Text)
            , "url" .= ("https://en.wikipedia.org/wiki/" <> T.replace " " "_" query :: Text)
            ]
  , toolTimeout = Just 10_000_000  -- 10 seconds for network request
  , toolRetryable = True
  }

--------------------------------------------------------------------------------
-- Web Scraper Tool
--------------------------------------------------------------------------------

-- | Web scraper tool for extracting content from URLs.
--   Input: {"url": "https://example.com", "selector": "h1"}
--   Output: {"content": "Page Title", "url": "https://example.com"}
--
--   TODO: Integrate with langchain-hs web scraper when available
webScraperTool :: Tool
webScraperTool = Tool
  { toolName = "web_scraper"
  , toolDescription = "Scrape content from web pages. Input: {\"url\": \"https://...\", \"selector\": \"CSS selector\"}. Returns extracted content."
  , toolSchema = ToolSchema
      { inputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "url" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("URL to scrape" :: Text)
                  ]
              , "selector" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("CSS selector for content extraction (optional)" :: Text)
                  ]
              ]
          , "required" .= (["url"] :: [Text])
          ]
      , outputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "content" .= object ["type" .= ("string" :: Text)]
              , "url" .= object ["type" .= ("string" :: Text)]
              ]
          ]
      }
  , toolExecute = \(ToolInput input) -> do
      case extractUrl input of
        Nothing -> return $ Left $ ToolExecutionError "Missing or invalid 'url' field"
        Just url -> do
          -- Stub implementation
          -- TODO: Implement actual web scraping via langchain-hs or http-conduit + scalpel
          let selector = maybe "body" id (extractSelector input)
          return $ Right $ ToolOutput $ object
            [ "content" .= ("Web scraper integration pending. URL: " <> url <> ", Selector: " <> selector :: Text)
            , "url" .= url
            ]
  , toolTimeout = Just 15_000_000  -- 15 seconds for network request
  , toolRetryable = True
  }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Extract expression from JSON input
extractExpression :: Value -> Maybe Text
extractExpression (Aeson.Object obj) = case KM.lookup "expression" obj of
  Just (Aeson.String expr) -> Just expr
  _ -> Nothing
extractExpression _ = Nothing

-- | Extract query from JSON input
extractQuery :: Value -> Maybe Text
extractQuery (Aeson.Object obj) = case KM.lookup "query" obj of
  Just (Aeson.String query) -> Just query
  _ -> Nothing
extractQuery _ = Nothing

-- | Extract URL from JSON input
extractUrl :: Value -> Maybe Text
extractUrl (Aeson.Object obj) = case KM.lookup "url" obj of
  Just (Aeson.String url) -> Just url
  _ -> Nothing
extractUrl _ = Nothing

-- | Extract selector from JSON input
extractSelector :: Value -> Maybe Text
extractSelector (Aeson.Object obj) = case KM.lookup "selector" obj of
  Just (Aeson.String sel) -> Just sel
  _ -> Nothing
extractSelector _ = Nothing

-- | Simple expression evaluator for basic arithmetic (stub)
--   TODO: Replace with proper expression parser or langchain-hs calculator
evaluateSimpleExpression :: Text -> IO Double
evaluateSimpleExpression expr = do
  -- Very basic implementation - just return 42 as placeholder
  -- TODO: Implement proper arithmetic evaluation
  return 42.0
