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

-- Simpler monad stack: ReaderT Config (StateT AppState (ExceptT Text IO))
type AppM = ReaderT Config (StateT AppState (ExceptT Text IO))

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
    { appName = "MTL Test App",
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
testReader :: AppM Text
testReader = do
  config <- ask
  return $
    "App: "
      <> appName config
      <> ", Debug: "
      <> T.pack (show (debugMode config))
      <> ", Max Retries: "
      <> T.pack (show (maxRetries config))

-- Test function using State monad
testState :: AppM Int
testState = do
  modify $ \s -> s {counter = counter s + 1}
  modify $ \s -> s {lastOperation = "increment"}
  state <- get
  return $ counter state

-- Test function using Except monad
testExcept :: Bool -> AppM Text
testExcept shouldFail = do
  if shouldFail
    then throwError "This is a test error"
    else return "Success!"

-- Test function using IO
testIO :: AppM Int
testIO = do
  randomNum <- liftIO $ randomRIO (1, 100)
  liftIO $ TIO.putStrLn $ "Generated random number: " <> T.pack (show randomNum)
  return randomNum

-- Complex function combining multiple transformers
complexOperation :: Text -> AppM Text
complexOperation input = do
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
      (\err -> return $ "Recovered from error: " <> err)

  -- Use State again
  modify $ \s -> s {lastOperation = "complex_operation"}
  state <- get

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

-- Main function
main :: IO ()
main = do
  TIO.putStrLn "=== MTL Compilation Test ==="
  TIO.putStrLn ""

  -- Run our monad stack
  result <- runExceptT $ runStateT (runReaderT mainApp initialConfig) initialState

  case result of
    Left err -> do
      TIO.putStrLn $ "Error: " <> err
    Right (_, finalState) -> do
      TIO.putStrLn "=== Final State ==="
      TIO.putStrLn $ T.pack (show finalState)
      TIO.putStrLn ""
      TIO.putStrLn "=== MTL Test Completed Successfully! ==="

-- Main application logic
mainApp :: AppM ()
mainApp = do
  -- Test Reader
  readerResult <- testReader
  liftIO $ TIO.putStrLn $ "Reader test: " <> readerResult

  -- Test State
  stateResult <- testState
  liftIO $ TIO.putStrLn $ "State test: Counter = " <> T.pack (show stateResult)

  -- Test Except (success case)
  exceptResult <- testExcept False
  liftIO $ TIO.putStrLn $ "Except test (success): " <> exceptResult

  -- Test Except (failure case with recovery)
  exceptResult2 <-
    catchError
      (testExcept True)
      (\err -> return $ "Recovery successful: " <> err)
  liftIO $ TIO.putStrLn $ "Except test (recovery): " <> exceptResult2

  -- Test IO
  ioResult <- testIO
  liftIO $ TIO.putStrLn $ "IO test: Random number = " <> T.pack (show ioResult)

  -- Test complex operation
  complexResult <- complexOperation "test-input"
  liftIO $ TIO.putStrLn $ "Complex operation: " <> complexResult
