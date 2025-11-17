{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Tool.File
-- Description : Built-in file operation tools
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides built-in tools for file operations that agents can use.
--
-- = Usage
--
-- @
-- import AgenticFramework.Tool.File
-- import AgenticFramework.Agent
--
-- main :: IO ()
-- main = do
--   let config = AgentConfig
--         { configTools = [readFileTool, writeFileTool, listDirectoryTool]
--         , ...
--         }
--   agent <- createAgent config
--   result <- executeAgent agent "Read the contents of config.json"
-- @
module AgenticFramework.Tool.File
  ( readFileTool,
    writeFileTool,
    listDirectoryTool,
    getFileInfoTool,
  )
where

import AgenticFramework.Types
import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory)
import System.FilePath (takeExtension, takeFileName)

--------------------------------------------------------------------------------
-- Read File Tool
--------------------------------------------------------------------------------

-- | Tool for reading file contents.
--   Input: {"path": "file.txt"}
--   Output: {"content": "file contents here", "size": 1234}
readFileTool :: Tool
readFileTool =
  Tool
    { toolName = "read_file",
      toolDescription = "Read the contents of a file. Input: {\"path\": \"filepath\"}. Returns file contents as text.",
      toolSchema =
        ToolSchema
          { inputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "path"
                          .= object
                            [ "type" .= ("string" :: Text),
                              "description" .= ("Path to the file to read" :: Text)
                            ]
                      ],
                  "required" .= (["path"] :: [Text])
                ],
            outputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "content" .= object ["type" .= ("string" :: Text)],
                        "size" .= object ["type" .= ("integer" :: Text)]
                      ]
                ]
          },
      toolExecute = \(ToolInput input) -> do
        case extractPath input of
          Nothing -> return $ Left $ ToolExecutionError "Missing or invalid 'path' field"
          Just filePath -> do
            result <- try $ TIO.readFile (T.unpack filePath)
            case result of
              Left (err :: SomeException) ->
                return $ Left $ ToolExecutionError $ T.pack $ "Failed to read file: " ++ show err
              Right content -> do
                let size = T.length content
                return $
                  Right $
                    ToolOutput $
                      object
                        [ "content" .= content,
                          "size" .= size
                        ],
      toolTimeout = Just 5_000_000, -- 5 seconds
      toolRetryable = True
    }

--------------------------------------------------------------------------------
-- Write File Tool
--------------------------------------------------------------------------------

-- | Tool for writing content to a file.
--   Input: {"path": "file.txt", "content": "text to write"}
--   Output: {"success": true, "bytes_written": 1234}
writeFileTool :: Tool
writeFileTool =
  Tool
    { toolName = "write_file",
      toolDescription = "Write content to a file. Input: {\"path\": \"filepath\", \"content\": \"text\"}. Creates or overwrites the file.",
      toolSchema =
        ToolSchema
          { inputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "path"
                          .= object
                            [ "type" .= ("string" :: Text),
                              "description" .= ("Path to the file to write" :: Text)
                            ],
                        "content"
                          .= object
                            [ "type" .= ("string" :: Text),
                              "description" .= ("Content to write to the file" :: Text)
                            ]
                      ],
                  "required" .= (["path", "content"] :: [Text])
                ],
            outputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "success" .= object ["type" .= ("boolean" :: Text)],
                        "bytes_written" .= object ["type" .= ("integer" :: Text)]
                      ]
                ]
          },
      toolExecute = \(ToolInput input) -> do
        case (extractPath input, extractContent input) of
          (Just filePath, Just content) -> do
            result <- try $ TIO.writeFile (T.unpack filePath) content
            case result of
              Left (err :: SomeException) ->
                return $ Left $ ToolExecutionError $ T.pack $ "Failed to write file: " ++ show err
              Right () ->
                return $
                  Right $
                    ToolOutput $
                      object
                        [ "success" .= True,
                          "bytes_written" .= T.length content
                        ]
          (Nothing, _) -> return $ Left $ ToolExecutionError "Missing or invalid 'path' field"
          (_, Nothing) -> return $ Left $ ToolExecutionError "Missing or invalid 'content' field",
      toolTimeout = Just 5_000_000, -- 5 seconds
      toolRetryable = False -- Don't retry writes to avoid duplicates
    }

--------------------------------------------------------------------------------
-- List Directory Tool
--------------------------------------------------------------------------------

-- | Tool for listing directory contents.
--   Input: {"path": "directory"}
--   Output: {"files": ["file1.txt", "file2.txt"], "count": 2}
listDirectoryTool :: Tool
listDirectoryTool =
  Tool
    { toolName = "list_directory",
      toolDescription = "List the contents of a directory. Input: {\"path\": \"directory_path\"}. Returns list of file and directory names.",
      toolSchema =
        ToolSchema
          { inputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "path"
                          .= object
                            [ "type" .= ("string" :: Text),
                              "description" .= ("Path to the directory to list" :: Text)
                            ]
                      ],
                  "required" .= (["path"] :: [Text])
                ],
            outputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "files"
                          .= object
                            [ "type" .= ("array" :: Text),
                              "items" .= object ["type" .= ("string" :: Text)]
                            ],
                        "count" .= object ["type" .= ("integer" :: Text)]
                      ]
                ]
          },
      toolExecute = \(ToolInput input) -> do
        case extractPath input of
          Nothing -> return $ Left $ ToolExecutionError "Missing or invalid 'path' field"
          Just dirPath -> do
            result <- try $ listDirectory (T.unpack dirPath)
            case result of
              Left (err :: SomeException) ->
                return $ Left $ ToolExecutionError $ T.pack $ "Failed to list directory: " ++ show err
              Right files -> do
                let fileNames = map T.pack files
                return $
                  Right $
                    ToolOutput $
                      object
                        [ "files" .= fileNames,
                          "count" .= length fileNames
                        ],
      toolTimeout = Just 5_000_000, -- 5 seconds
      toolRetryable = True
    }

--------------------------------------------------------------------------------
-- Get File Info Tool
--------------------------------------------------------------------------------

-- | Tool for getting file metadata.
--   Input: {"path": "file.txt"}
--   Output: {"exists": true, "is_file": true, "is_directory": false, "size": 1234}
getFileInfoTool :: Tool
getFileInfoTool =
  Tool
    { toolName = "get_file_info",
      toolDescription = "Get metadata about a file or directory. Input: {\"path\": \"filepath\"}. Returns existence, type, and size information.",
      toolSchema =
        ToolSchema
          { inputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "path"
                          .= object
                            [ "type" .= ("string" :: Text),
                              "description" .= ("Path to the file or directory" :: Text)
                            ]
                      ],
                  "required" .= (["path"] :: [Text])
                ],
            outputSchema =
              object
                [ "type" .= ("object" :: Text),
                  "properties"
                    .= object
                      [ "exists" .= object ["type" .= ("boolean" :: Text)],
                        "is_file" .= object ["type" .= ("boolean" :: Text)],
                        "is_directory" .= object ["type" .= ("boolean" :: Text)],
                        "size" .= object ["type" .= ("integer" :: Text)]
                      ]
                ]
          },
      toolExecute = \(ToolInput input) -> do
        case extractPath input of
          Nothing -> return $ Left $ ToolExecutionError "Missing or invalid 'path' field"
          Just filePath -> do
            let path = T.unpack filePath
            isFile <- doesFileExist path
            isDir <- doesDirectoryExist path
            let exists = isFile || isDir

            if not exists
              then
                return $
                  Right $
                    ToolOutput $
                      object
                        [ "exists" .= False,
                          "is_file" .= False,
                          "is_directory" .= False,
                          "size" .= (0 :: Int)
                        ]
              else
                if isFile
                  then do
                    size <- getFileSize path
                    return $
                      Right $
                        ToolOutput $
                          object
                            [ "exists" .= True,
                              "is_file" .= True,
                              "is_directory" .= False,
                              "size" .= size
                            ]
                  else
                    return $
                      Right $
                        ToolOutput $
                          object
                            [ "exists" .= True,
                              "is_file" .= False,
                              "is_directory" .= True,
                              "size" .= (0 :: Int)
                            ],
      toolTimeout = Just 3_000_000, -- 3 seconds
      toolRetryable = True
    }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Extract path from JSON input
extractPath :: Value -> Maybe Text
extractPath (Aeson.Object obj) = case KM.lookup "path" obj of
  Just (Aeson.String path) -> Just path
  _ -> Nothing
extractPath _ = Nothing

-- | Extract content from JSON input
extractContent :: Value -> Maybe Text
extractContent (Aeson.Object obj) = case KM.lookup "content" obj of
  Just (Aeson.String content) -> Just content
  _ -> Nothing
extractContent _ = Nothing
