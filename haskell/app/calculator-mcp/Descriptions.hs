module Descriptions (descriptions) where

-- Custom descriptions for better MCP integration
descriptions :: [(String, String)]
descriptions = 
    [ -- Tool descriptions
      ("Add", "Add two numbers together")
    , ("Subtract", "Subtract the second number from the first")
    , ("Multiply", "Multiply two numbers")
    , ("Divide", "Divide the first number by the second")
    , ("Power", "Raise the base number to the given exponent")
    , ("Sqrt", "Calculate the square root of a number")
    , ("a", "First number for the operation")
    , ("b", "Second number for the operation")
    , ("base", "The base number to raise to a power")
    , ("exponent", "The exponent to raise the base to")
    , ("value", "The number to calculate the square root of")
    , -- Prompt descriptions
      ("CalculateExpression", "Get help with calculating mathematical expressions")
    , ("GetHelp", "Show help information for the calculator")
    , ("expression", "The mathematical expression you want help with")
    , -- Resource descriptions
      ("Operations", "List of supported mathematical operations")
    , ("Constants", "Common mathematical constants")
    ] 