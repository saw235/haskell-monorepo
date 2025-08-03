{-# LANGUAGE FlexibleContexts #-}

module WithoutMonadTransformers where

import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (State, get, put, runState)

-- ============================================================================
-- PROBLEM 1: Without Monad Transformers - Manual State Passing
-- ============================================================================

-- Let's try to implement the same functionality without transformers
-- We need to manually pass state around - this gets messy quickly!

-- Version 1: Just Reader (can't have state)
readNameWithoutState :: Reader String String
readNameWithoutState = do
  name <- ask
  return $ "Hello, " ++ name

-- Version 2: Just State (can't read configuration)
incrementCounterWithoutReader :: State Int Int
incrementCounterWithoutReader = do
  current <- get
  let newValue = current + 1
  put newValue
  return newValue

-- ============================================================================
-- PROBLEM 2: Manual State Threading (The Ugly Way)
-- ============================================================================

-- To combine both effects, we have to manually thread state through functions
-- This is error-prone and hard to read!

-- We need to pass state explicitly in every function
greetAndCountManual :: String -> Int -> (String, Int)
greetAndCountManual name currentCount = 
  let greeting = "Hello " ++ name ++ "! You are visitor #" ++ show (currentCount + 1)
      newCount = currentCount + 1
  in (greeting, newCount)

-- If we want to chain multiple operations, it gets worse:
greetMultiplePeopleManual :: String -> String -> Int -> (String, Int)
greetMultiplePeopleManual name1 name2 initialCount = 
  let (greeting1, count1) = greetAndCountManual name1 initialCount
      (greeting2, count2) = greetAndCountManual name2 count1
      combinedGreeting = greeting1 ++ "\n" ++ greeting2
  in (combinedGreeting, count2)

-- ============================================================================
-- PROBLEM 3: Error-Prone Manual State Management
-- ============================================================================

-- Look how easy it is to make mistakes when manually threading state:

-- BUGGY VERSION: Forgot to update the counter!
greetAndCountBuggy :: String -> Int -> (String, Int)
greetAndCountBuggy name currentCount = 
  let greeting = "Hello " ++ name ++ "! You are visitor #" ++ show (currentCount + 1)
      -- Oops! Forgot to increment the counter
      newCount = currentCount  -- BUG: should be currentCount + 1
  in (greeting, newCount)

-- BUGGY VERSION: Using wrong counter value in greeting
greetAndCountBuggy2 :: String -> Int -> (String, Int)
greetAndCountBuggy2 name currentCount = 
  let newCount = currentCount + 1
      -- BUG: Using newCount instead of currentCount in the greeting
      greeting = "Hello " ++ name ++ "! You are visitor #" ++ show newCount
  in (greeting, newCount)

-- ============================================================================
-- PROBLEM 4: Adding More Effects Makes It Even Worse
-- ============================================================================

-- What if we also want logging? Now we need to thread THREE things around!

-- Manual threading with logging
greetAndCountWithLoggingManual :: String -> Int -> [String] -> (String, Int, [String])
greetAndCountWithLoggingManual name currentCount logs = 
  let newCount = currentCount + 1
      greeting = "Hello " ++ name ++ "! You are visitor #" ++ show (currentCount + 1)
      newLogs = logs ++ ["Greeted " ++ name ++ " (visitor #" ++ show (currentCount + 1) ++ ")"]
  in (greeting, newCount, newLogs)

-- ============================================================================
-- PROBLEM 5: Type Safety Issues
-- ============================================================================

-- Without monad transformers, the type system can't help us ensure we're
-- handling state correctly. We can easily forget to pass state around:

-- This function claims to return a greeting but doesn't handle state at all
-- The type system can't catch this mistake!
greetWithoutStateHandling :: String -> String
greetWithoutStateHandling name = "Hello " ++ name ++ "!"

-- ============================================================================
-- DEMONSTRATION: Running the Manual Versions
-- ============================================================================

demoManualApproach :: IO ()
demoManualApproach = do
  putStrLn "=== WITHOUT Monad Transformers (Manual State Threading) ==="
  putStrLn ""
  
  putStrLn "--- Manual State Threading ---"
  let (result1, count1) = greetAndCountManual "Alice" 0
  putStrLn $ "1. " ++ result1
  putStrLn $ "   Counter: " ++ show count1
  putStrLn ""
  
  let (result2, count2) = greetMultiplePeopleManual "Bob" "Charlie" 0
  putStrLn $ "2. Multiple greetings:"
  putStrLn result2
  putStrLn $ "   Final counter: " ++ show count2
  putStrLn ""
  
  putStrLn "--- The Problems ---"
  putStrLn "1. Manual state threading is error-prone"
  putStrLn "2. Easy to forget to pass state around"
  putStrLn "3. Type system can't help catch mistakes"
  putStrLn "4. Adding more effects makes it exponentially worse"
  putStrLn "5. Code becomes hard to read and maintain"
  putStrLn ""
  
  putStrLn "--- Compare with Monad Transformers ---"
  putStrLn "Monad transformers solve these problems by:"
  putStrLn "1. Automatically threading state through computations"
  putStrLn "2. Type system ensures you can't forget to handle effects"
  putStrLn "3. Clean, composable syntax"
  putStrLn "4. Easy to add/remove effects"
  putStrLn "5. Compile-time safety guarantees" 