{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import MCP.Server
import MCP.Server.Derive
import Data.Text (Text)
import qualified Data.Text as T
import Descriptions (descriptions)
import Types
import Network.URI (URI)

-- Implement tool handlers
handleCalculatorTool :: CalculatorTool -> IO Content
handleCalculatorTool (Add a b) = 
    pure $ ContentText $ T.pack $ show (a + b)
handleCalculatorTool (Subtract a b) = 
    pure $ ContentText $ T.pack $ show (a - b)
handleCalculatorTool (Multiply a b) = 
    pure $ ContentText $ T.pack $ show (a * b)
handleCalculatorTool (Divide a b) = 
    if b == 0 
        then pure $ ContentText "Error: Division by zero"
        else pure $ ContentText $ T.pack $ show (a / b)
handleCalculatorTool (Power base exponent) = 
    pure $ ContentText $ T.pack $ show (base ** exponent)
handleCalculatorTool (Sqrt value) = 
    if value < 0 
        then pure $ ContentText "Error: Cannot take square root of negative number"
        else pure $ ContentText $ T.pack $ show (sqrt value)

-- Implement prompt handlers
handleCalculatorPrompt :: CalculatorPrompt -> IO Content
handleCalculatorPrompt (CalculateExpression expr) = 
    pure $ ContentText $ "I can help you calculate: " <> expr <> "\nUse the calculator tools for precise arithmetic operations."
handleCalculatorPrompt GetHelp = 
    pure $ ContentText $ T.unlines
        [ "Calculator MCP Server Help"
        , ""
        , "Available Tools:"
        , "- add: Add two numbers"
        , "- subtract: Subtract two numbers"
        , "- multiply: Multiply two numbers"
        , "- divide: Divide two numbers"
        , "- power: Raise a number to a power"
        , "- sqrt: Calculate square root"
        , ""
        , "Available Resources:"
        , "- operations: List of supported operations"
        , "- constants: Mathematical constants"
        , ""
        , "Available Prompts:"
        , "- calculate_expression: Get help with expressions"
        , "- get_help: Show this help message"
        ]

-- Implement resource handlers
handleCalculatorResource :: URI -> CalculatorResource -> IO ResourceContent
handleCalculatorResource uri Operations = 
    pure $ ResourceText
        uri
        "text/plain"
        (T.unlines
            [ "Supported Operations:"
            , ""
            , "1. Addition (+) - add two numbers"
            , "2. Subtraction (-) - subtract two numbers"
            , "3. Multiplication (*) - multiply two numbers"
            , "4. Division (/) - divide two numbers"
            , "5. Exponentiation (^) - raise to power"
            , "6. Square Root (√) - calculate square root"
            , ""
            , "All operations work with real numbers (Double precision)."
            ])
handleCalculatorResource uri Constants = 
    pure $ ResourceText
        uri
        "text/plain"
        (T.unlines
            [ "Mathematical Constants:"
            , ""
            , "π (pi) ≈ 3.141592653589793"
            , "e ≈ 2.718281828459045"
            , "φ (golden ratio) ≈ 1.618033988749895"
            , "√2 ≈ 1.4142135623730951"
            , "√3 ≈ 1.7320508075688772"
            , ""
            , "Note: These are approximate values. For precise calculations, use the calculator tools."
            ])

main :: IO ()
main = do
    putStrLn "Starting Calculator MCP Server..."
    runMcpServerStdio serverInfo handlers
  where
    serverInfo = McpServerInfo
      { serverName = "Calculator MCP Server"
      , serverVersion = "1.0.0"
      , serverInstructions = "A calculator MCP server that provides tools for basic arithmetic operations, mathematical prompts, and resources."
      }
    handlers = McpServerHandlers
      { prompts = Just $(derivePromptHandlerWithDescription ''CalculatorPrompt 'handleCalculatorPrompt descriptions)
      , resources = Just $(deriveResourceHandlerWithDescription ''CalculatorResource 'handleCalculatorResource descriptions)
      , tools = Just $(deriveToolHandlerWithDescription ''CalculatorTool 'handleCalculatorTool descriptions)
      } 