{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT, get, modify, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (randomRIO)

-- Advanced monad stack: ReaderT Config (StateT AppState (WriterT [Text] (ExceptT Text IO)))
type AdvancedAppM = ReaderT Config (StateT AppState (WriterT [Text] (ExceptT Text IO)))

-- Configuration data
data Config = Config
  { appName :: Text,
    debugMode :: Bool,
    maxRetries :: Int
  }
  deriving (Show)

-- Application state
data AppState = AppState
  { counter :: Int,
    lastOperation :: Text,
    userInputs :: [Text]
  }
  deriving (Show)

-- Initial configuration
initialConfig :: Config
initialConfig =
  Config
    { appName = "Advanced MTL Test App",
      debugMode = True,
      maxRetries = 3
    }

-- Initial state
initialState :: AppState
initialState =
  AppState
    { counter = 0,
      lastOperation = "initialized",
      userInputs = []
    }

-- Test function using Reader monad
testReader :: AdvancedAppM Text
testReader = do
  config <- ask
  tell ["Reading configuration"]
  return $
    "App: "
      <> appName config
      <> ", Debug: "
      <> T.pack (show (debugMode config))
      <> ", Max Retries: "
      <> T.pack (show (maxRetries config))

-- Test function using State monad
testState :: AdvancedAppM Int
testState = do
  tell ["Modifying state"]
  modify $ \s -> s {counter = counter s + 1}
  modify $ \s -> s {lastOperation = "increment"}
  state <- get
  return $ counter state

-- Test function using Writer monad
testWriter :: AdvancedAppM ()
testWriter = do
  tell ["Writing log messages"]
  tell ["This is a test message"]
  tell ["Another log entry"]
  return ()

-- Test function using Except monad
testExcept :: Bool -> AdvancedAppM Text
testExcept shouldFail = do
  if shouldFail
    then do
      tell ["Throwing error"]
      throwError "This is a test error"
    else do
      tell ["Operation successful"]
      return "Success!"

-- Test function using IO
testIO :: AdvancedAppM Int
testIO = do
  tell ["Performing IO operation"]
  randomNum <- liftIO $ randomRIO (1, 100)
  liftIO $ TIO.putStrLn $ "Generated random number: " <> T.pack (show randomNum)
  return randomNum

-- Complex function combining multiple transformers
complexOperation :: Text -> AdvancedAppM Text
complexOperation input = do
  tell ["Starting complex operation"]

  -- Use Reader
  config <- ask
  let retries = maxRetries config

  -- Use State
  modify $ \s -> s {userInputs = input : userInputs s}

  -- Use IO
  randomNum <- testIO

  -- Use Except with error handling
  result <-
    catchError
      (testExcept False)
      ( \err -> do
          tell ["Caught error: " <> err]
          return "Recovered from error"
      )

  -- Use State again
  modify $ \s -> s {lastOperation = "complex_operation"}
  state <- get

  tell ["Complex operation completed"]

  return $
    T.concat
      [ "Input: ",
        input,
        ", Random: ",
        T.pack (show randomNum),
        ", Result: ",
        result,
        ", Counter: ",
        T.pack (show (counter state))
      ]

-- Test nested monad transformers
testNestedTransformers :: AdvancedAppM ()
testNestedTransformers = do
  tell ["Testing nested transformers"]

  -- Simulate a nested operation
  let nestedOp = do
        tell ["Nested: Reading config"]
        config <- ask
        tell ["Nested: Modifying state"]
        modify $ \s -> s {counter = counter s + 10}
        tell ["Nested: Writing to log"]
        tell ["Nested: Operation complete"]

  -- Run the nested operation
  nestedOp

  tell ["Nested transformers test completed"]

-- Main function
main :: IO ()
main = do
  TIO.putStrLn "=== Advanced MTL Compilation Test ==="
  TIO.putStrLn ""

  -- Run our monad stack
  result <- runExceptT $ runWriterT $ runStateT (runReaderT mainApp initialConfig) initialState

  case result of
    Left err -> do
      TIO.putStrLn $ "Error: " <> err
    Right (((), finalState), logs) -> do
      TIO.putStrLn "=== Execution Log ==="
      mapM_ TIO.putStrLn logs
      TIO.putStrLn ""
      TIO.putStrLn "=== Final State ==="
      TIO.putStrLn $ T.pack (show finalState)
      TIO.putStrLn ""
      TIO.putStrLn "=== Advanced MTL Test Completed Successfully! ==="

-- Main application logic
mainApp :: AdvancedAppM ()
mainApp = do
  tell ["Starting advanced MTL test application"]

  -- Test Reader
  readerResult <- testReader
  liftIO $ TIO.putStrLn $ "Reader test: " <> readerResult

  -- Test State
  stateResult <- testState
  liftIO $ TIO.putStrLn $ "State test: Counter = " <> T.pack (show stateResult)

  -- Test Writer
  testWriter
  liftIO $ TIO.putStrLn "Writer test: Check logs above"

  -- Test Except (success case)
  exceptResult <- testExcept False
  liftIO $ TIO.putStrLn $ "Except test (success): " <> exceptResult

  -- Test Except (failure case with recovery)
  exceptResult2 <-
    catchError
      (testExcept True)
      ( \err -> do
          tell ["Recovered from error: " <> err]
          return "Recovery successful"
      )
  liftIO $ TIO.putStrLn $ "Except test (recovery): " <> exceptResult2

  -- Test IO
  ioResult <- testIO
  liftIO $ TIO.putStrLn $ "IO test: Random number = " <> T.pack (show ioResult)

  -- Test complex operation
  complexResult <- complexOperation "test-input"
  liftIO $ TIO.putStrLn $ "Complex operation: " <> complexResult

  -- Test nested transformers
  testNestedTransformers

  tell ["Advanced MTL test application completed"]
