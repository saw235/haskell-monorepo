{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Tool.WebSearch
-- Description : Web search tool implementation
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides web search capabilities for agents.
--
-- Note: This is a placeholder implementation. Full web search integration
-- will require API keys and integration with services like Google Custom Search,
-- DuckDuckGo, or Brave Search.
module AgenticFramework.Tool.WebSearch
  ( webSearchTool,
  )
where

import AgenticFramework.Types
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Web Search Tool
--------------------------------------------------------------------------------

-- | Web search tool using configured search API.
--   Input: {"query": "Haskell functional programming", "num_results": 5}
--   Output: {"results": [...], "query": "...", "count": 5}
--
--   TODO: Integrate with actual search API (Google Custom Search, DuckDuckGo, or Brave)
--   TODO: Add support for API key configuration
--   TODO: Add result filtering and ranking
webSearchTool :: Tool
webSearchTool =
  Tool
    { toolName = "web_search",
      toolDescription = "Search the web for information. Input: {\"query\": \"search query\", \"num_results\": 5}. Returns search results with titles, URLs, and snippets.",
      toolSchema =
        ToolSchema
          { inputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "query"
                          .= object
                            [ "type" .= ("string" :: Text),
                              "description" .= ("Search query" :: Text)
                            ],
                        "num_results"
                          .= object
                            [ "type" .= ("integer" :: Text),
                              "description" .= ("Number of results to return (default: 5)" :: Text),
                              "default" .= (5 :: Int)
                            ]
                      ],
                  "required" .= (["query"] :: [Text])
                ],
            outputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "results"
                          .= object
                            [ "type" .= ("array" :: Text),
                              "items"
                                .= object
                                  [ "type" .= ("object" :: Text),
                                    "properties"
                                      .= object
                                        [ "title" .= object ["type" .= ("string" :: Text)],
                                          "url" .= object ["type" .= ("string" :: Text)],
                                          "snippet" .= object ["type" .= ("string" :: Text)]
                                        ]
                                  ]
                            ],
                        "query" .= object ["type" .= ("string" :: Text)],
                        "count" .= object ["type" .= ("integer" :: Text)]
                      ]
                ]
          },
      toolExecute = \(ToolInput input) -> do
        case extractQuery input of
          Nothing -> return $ Left $ ToolExecutionError "Missing or invalid 'query' field"
          Just query -> do
            let numResults = extractNumResults input
            -- Stub implementation
            -- TODO: Implement actual web search API call
            let stubResults = createStubResults query numResults
            return $
              Right $
                ToolOutput $
                  object
                    [ "results" .= stubResults,
                      "query" .= query,
                      "count" .= length stubResults
                    ],
      toolTimeout = Just 15_000_000, -- 15 seconds for network request
      toolRetryable = True
    }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Extract query from JSON input
extractQuery :: Value -> Maybe Text
extractQuery (Aeson.Object obj) = case KM.lookup "query" obj of
  Just (Aeson.String query) -> Just query
  _ -> Nothing
extractQuery _ = Nothing

-- | Extract num_results from JSON input (default: 5)
extractNumResults :: Value -> Int
extractNumResults (Aeson.Object obj) = case KM.lookup "num_results" obj of
  Just (Aeson.Number n) -> floor n
  _ -> 5
extractNumResults _ = 5

-- | Create stub search results (placeholder)
createStubResults :: Text -> Int -> [Value]
createStubResults query numResults =
  take numResults $ map (createStubResult query) [1 .. numResults]

-- | Create a single stub search result
createStubResult :: Text -> Int -> Value
createStubResult query n =
  object
    [ "title" .= (query <> " - Result " <> T.pack (show n) :: Text),
      "url" .= ("https://example.com/result" <> T.pack (show n) :: Text),
      "snippet" .= ("This is a placeholder search result for: " <> query :: Text)
    ]
