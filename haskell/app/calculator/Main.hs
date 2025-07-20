module Main where

import System.Environment (getArgs)
import System.IO
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [num1Str, num2Str, op] ->
      -- Non-interactive mode with command line arguments
      case (readMaybe num1Str :: Maybe Double, readMaybe num2Str :: Maybe Double) of
        (Just n1, Just n2) -> do
          let result = calculate n1 n2 op
          putStrLn $ "Result: " ++ show result
        _ -> putStrLn "Invalid numbers provided"
    [] ->
      -- Interactive mode
      interactiveMode
    _ -> do
      putStrLn "Usage: calculator [number1] [number2] [operation]"
      putStrLn "Operations: +, -, *, /"
      putStrLn "Example: calculator 10 5 +"
      putStrLn "Or run without arguments for interactive mode"

interactiveMode :: IO ()
interactiveMode = do
  putStrLn "Simple Calculator"
  putStrLn "Enter two numbers and an operation (+, -, *, /):"

  putStr "First number: "
  hFlush stdout
  num1Str <- getLine

  putStr "Second number: "
  hFlush stdout
  num2Str <- getLine

  putStr "Operation (+, -, *, /): "
  hFlush stdout
  op <- getLine

  case (readMaybe num1Str :: Maybe Double, readMaybe num2Str :: Maybe Double) of
    (Just n1, Just n2) -> do
      let result = calculate n1 n2 op
      putStrLn $ "Result: " ++ show result
    _ -> putStrLn "Invalid numbers entered"

calculate :: Double -> Double -> String -> Double
calculate n1 n2 op = case op of
  "+" -> n1 + n2
  "-" -> n1 - n2
  "*" -> n1 * n2
  "/" -> if n2 /= 0 then n1 / n2 else error "Division by zero"
  _ -> error "Invalid operation"
