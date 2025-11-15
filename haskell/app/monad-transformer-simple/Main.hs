{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)

-- ============================================================================
-- CORE PRINCIPLE: Monad Transformers let you combine 2 effects safely
-- ============================================================================

-- Think of each transformer as adding a "capability" to your computation
-- Just like how you can stack LEGO blocks, you can stack monad transformers

-- ============================================================================
-- STEP 1: Individual Effects (Simple Monads)
-- ============================================================================

-- Effect 1: Reading a value (like reading from a config file)
type SimpleReader = ReaderT String IO

-- Effect 2: Keeping track of a number (like a counter)
type SimpleState = StateT Int IO

-- ============================================================================
-- STEP 2: Simple Examples of Each Effect
-- ============================================================================

-- Reader: "I need to read a name"
readName :: SimpleReader String
readName = do
  name <- ask -- ask gets the value that was passed in
  return $ "Hello, " ++ name

-- State: "I need to count something"
incrementCounter :: SimpleState Int
incrementCounter = do
  current <- get -- get reads the current state
  let newValue = current + 1
  put newValue -- put writes the new state
  return newValue

-- ============================================================================
-- STEP 3: Combining Two Effects (The Magic!)
-- ============================================================================

-- Combining Reader + State: "I need to read a name AND keep a counter"
type ReaderWithState = ReaderT String (StateT Int IO)

-- This function can both read a name AND update a counter
greetAndCount :: ReaderWithState String
greetAndCount = do
  name <- ask -- "I need the name from my environment"
  current <- get -- "I need the current count from my notebook"
  put (current + 1) -- "I'm writing the new count to my notebook"
  return $ "Hello " ++ name ++ "! You are visitor #" ++ show (current + 1)

-- ============================================================================
-- STEP 4: Running the Examples
-- ============================================================================

main :: IO ()
main = do
  putStrLn "=== Simple Monad Transformer Examples (2 Effects Only) ==="
  putStrLn ""

  -- Test individual effects
  putStrLn "--- Individual Effects ---"

  -- Reader
  putStrLn "1. Reader Effect (Reading a name):"
  result1 <- runReaderT readName "Alice"
  putStrLn $ "   " ++ result1
  putStrLn ""

  -- State
  putStrLn "2. State Effect (Counting):"
  (result2, finalCount) <- runStateT incrementCounter 0
  putStrLn $ "   Counter is now: " ++ show finalCount
  putStrLn ""

  -- Test combining two effects
  putStrLn "--- Combining Two Effects ---"
  putStrLn "3. Reader + State (Reading name + counting):"
  (result3, finalCount3) <- runStateT (runReaderT greetAndCount "Bob") 0
  putStrLn $ "   " ++ result3
  putStrLn $ "   Final counter: " ++ show finalCount3
  putStrLn ""

  putStrLn "=== The Core Principle ==="
  putStrLn ""
  putStrLn "Monad transformers let you:"
  putStrLn "1. Take simple effects (Reader, State)"
  putStrLn "2. Stack them together like LEGO blocks"
  putStrLn "3. Get a computation that can do BOTH safely"
  putStrLn ""
  putStrLn "The type system ensures you can't forget to handle state"
  putStrLn "or access configuration incorrectly. It's like having a safety net!"
  putStrLn ""

  -- Show the comparison with manual approach
  putStrLn "=== COMPARISON: With vs Without Monad Transformers ==="
  putStrLn ""

  putStrLn "WITH Monad Transformers (clean):"
  putStrLn "greetAndCount :: ReaderWithState String"
  putStrLn "greetAndCount = do"
  putStrLn "  name <- ask           -- Read configuration"
  putStrLn "  current <- get        -- Read state"
  putStrLn "  put (current + 1)     -- Update state"
  putStrLn "  return $ \"Hello \" ++ name ++ \"! You are visitor #\" ++ show (current + 1)"
  putStrLn ""

  putStrLn "WITHOUT Monad Transformers (messy):"
  putStrLn "greetAndCountManual :: String -> Int -> (String, Int)"
  putStrLn "greetAndCountManual name currentCount = "
  putStrLn "  let greeting = \"Hello \" ++ name ++ \"! You are visitor #\" ++ show (currentCount + 1)"
  putStrLn "      newCount = currentCount + 1"
  putStrLn "  in (greeting, newCount)"
  putStrLn ""

  putStrLn "=== Key Problems Monad Transformers Solve ==="
  putStrLn ""
  putStrLn "1. MANUAL STATE THREADING: Without transformers, you must manually"
  putStrLn "   pass state through every function call - error-prone!"
  putStrLn ""
  putStrLn "2. TYPE SAFETY: The type system can't catch if you forget to"
  putStrLn "   handle state or pass it around correctly"
  putStrLn ""
  putStrLn "3. COMPOSABILITY: Adding more effects (like logging) makes the"
  putStrLn "   manual approach exponentially worse"
  putStrLn ""
  putStrLn "4. READABILITY: Manual state threading makes code hard to read"
  putStrLn "   and understand"
  putStrLn ""
  putStrLn "5. MAINTAINABILITY: Changes require updating many function signatures"
  putStrLn ""
  putStrLn "=== Example completed! ==="
