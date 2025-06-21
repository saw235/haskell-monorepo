{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)

-- Define calculator tools
data CalculatorTool
    = Add { a :: Double, b :: Double }
    | Subtract { a :: Double, b :: Double }
    | Multiply { a :: Double, b :: Double }
    | Divide { a :: Double, b :: Double }
    | Power { base :: Double, exponent :: Double }
    | Sqrt { value :: Double }
    deriving (Show, Eq)

-- Define calculator prompts
data CalculatorPrompt
    = CalculateExpression { expression :: Text }
    | GetHelp
    deriving (Show, Eq)

-- Define calculator resources
data CalculatorResource
    = Operations
    | Constants
    deriving (Show, Eq) 