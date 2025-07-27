module Main where

import AbsCalc
import ErrMCalc
import LexCalc
import ParCalc

-- Simple evaluation function for the calculator expressions
eval :: Exp -> Integer
eval e = case e of
  EAdd e1 e2 -> eval e1 + eval e2
  ESub e1 e2 -> eval e1 - eval e2
  EMul e1 e2 -> eval e1 * eval e2
  EDiv e1 e2 -> eval e1 `div` eval e2
  EInt n -> n

-- Parse and evaluate a string expression
calculate :: String -> Either String Integer
calculate s = case pExp (myLexer s) of
  Bad err -> Left err
  Ok exp -> Right (eval exp)

main :: IO ()
main = do
  putStrLn "BNFC Calculator Example"
  putStrLn "Enter arithmetic expressions (Ctrl+C to exit):"
  loop

loop :: IO ()
loop = do
  putStr "> "
  input <- getLine
  case calculate input of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right result -> putStrLn $ "Result: " ++ show result
  loop
