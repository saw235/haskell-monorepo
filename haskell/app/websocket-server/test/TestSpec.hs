{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calculator
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import KeyboardHandler
import MessageHandler
import qualified System.Exit as Exit
import Test.HUnit
import Types

-- Message Response Tests
testCreateWelcomeResponse :: Test
testCreateWelcomeResponse = TestCase $ do
  let response = createResponse "welcome" "Hello from Haskell server!"
  let decoded = decode response :: Maybe ServerMessage
  assertEqual
    "Welcome response should decode correctly"
    (Just (ServerMessage "welcome" "Hello from Haskell server!"))
    decoded

testCreatePongResponse :: Test
testCreatePongResponse = TestCase $ do
  let response = createResponse "pong" "Pong from Haskell!"
  let decoded = decode response :: Maybe ServerMessage
  assertEqual
    "Pong response should decode correctly"
    (Just (ServerMessage "pong" "Pong from Haskell!"))
    decoded

testCreateEchoResponse :: Test
testCreateEchoResponse = TestCase $ do
  let response = createResponse "echo_response" "Echo: test message"
  let decoded = decode response :: Maybe ServerMessage
  assertEqual
    "Echo response should decode correctly"
    (Just (ServerMessage "echo_response" "Echo: test message"))
    decoded

-- Calculator Tests
testCalculatorAdditionWithSpaces :: Test
testCalculatorAdditionWithSpaces = TestCase $ do
  assertEqual "Addition with spaces" "15.0" (calculateExpression "10 + 5")

testCalculatorSubtractionWithSpaces :: Test
testCalculatorSubtractionWithSpaces = TestCase $ do
  assertEqual "Subtraction with spaces" "7.0" (calculateExpression "10 - 3")

testCalculatorMultiplicationWithSpaces :: Test
testCalculatorMultiplicationWithSpaces = TestCase $ do
  assertEqual "Multiplication with spaces" "28.0" (calculateExpression "4 * 7")

testCalculatorDivisionWithSpaces :: Test
testCalculatorDivisionWithSpaces = TestCase $ do
  assertEqual "Division with spaces" "5.0" (calculateExpression "15 / 3")

testCalculatorAdditionWithoutSpaces :: Test
testCalculatorAdditionWithoutSpaces = TestCase $ do
  assertEqual "Addition without spaces" "15.0" (calculateExpression "10+5")

testCalculatorSubtractionWithoutSpaces :: Test
testCalculatorSubtractionWithoutSpaces = TestCase $ do
  assertEqual "Subtraction without spaces" "7.0" (calculateExpression "10-3")

testCalculatorMultiplicationWithoutSpaces :: Test
testCalculatorMultiplicationWithoutSpaces = TestCase $ do
  assertEqual "Multiplication without spaces" "28.0" (calculateExpression "4*7")

testCalculatorDivisionWithoutSpaces :: Test
testCalculatorDivisionWithoutSpaces = TestCase $ do
  assertEqual "Division without spaces" "5.0" (calculateExpression "15/3")

testCalculatorDivisionByZero :: Test
testCalculatorDivisionByZero = TestCase $ do
  assertEqual "Division by zero" "Division by zero" (calculateExpression "10 / 0")

testCalculatorInvalidNumbers :: Test
testCalculatorInvalidNumbers = TestCase $ do
  assertEqual "Invalid numbers" "Invalid numbers" (calculateExpression "abc + 5")

testCalculatorInvalidFormat :: Test
testCalculatorInvalidFormat = TestCase $ do
  assertEqual
    "Invalid expression format"
    "Invalid expression format. Use: number operator number (e.g., '10 + 5' or '10+5')"
    (calculateExpression "just text")

-- Keyboard Handler Tests
testKeyboardHandlerValidInput :: Test
testKeyboardHandlerValidInput = TestCase $ do
  let keyJson = "{\"key\":\"ArrowUp\",\"direction\":\"up\",\"position\":{\"x\":100,\"y\":200}}"
  let expected = createResponse "keyboard_ack" "Key: ArrowUp (up) at position (100,200)"
  assertEqual "Valid keyboard input" expected (handleKeyboardInput keyJson)

testKeyboardHandlerInvalidInput :: Test
testKeyboardHandlerInvalidInput = TestCase $ do
  let expected = createResponse "keyboard_error" "Invalid keyboard data format"
  assertEqual "Invalid keyboard input" expected (handleKeyboardInput "invalid json")

testKeyboardHandlerMissingPosition :: Test
testKeyboardHandlerMissingPosition = TestCase $ do
  let keyJson = "{\"key\":\"ArrowDown\",\"direction\":\"down\"}"
  let expected = createResponse "keyboard_ack" "Key: ArrowDown (down) at position (0,0)"
  assertEqual "Missing position data" expected (handleKeyboardInput keyJson)

-- Message Handler Tests
testMessageHandlerPing :: Test
testMessageHandlerPing = TestCase $ do
  let msg = ClientMessage "ping" ""
  let expected = createResponse "pong" "Pong from Haskell!"
  assertEqual "Ping message handling" expected (handleClientMessage msg)

testMessageHandlerEcho :: Test
testMessageHandlerEcho = TestCase $ do
  let msg = ClientMessage "echo" "test content"
  let expected = createResponse "echo_response" "Echo: test content"
  assertEqual "Echo message handling" expected (handleClientMessage msg)

testMessageHandlerCalculate :: Test
testMessageHandlerCalculate = TestCase $ do
  let msg = ClientMessage "calculate" "10 + 5"
  let expected = createResponse "calculation_result" "15.0"
  assertEqual "Calculate message handling" expected (handleClientMessage msg)

testMessageHandlerHelp :: Test
testMessageHandlerHelp = TestCase $ do
  let msg = ClientMessage "help" ""
  let expected = createResponse "help_response" helpText
  assertEqual "Help message handling" expected (handleClientMessage msg)

testMessageHandlerKeyboard :: Test
testMessageHandlerKeyboard = TestCase $ do
  let keyData = "{\"key\":\"ArrowLeft\",\"direction\":\"left\",\"position\":{\"x\":50,\"y\":75}}"
  let msg = ClientMessage "keyboard" keyData
  let expected = createResponse "keyboard_ack" "Key: ArrowLeft (left) at position (50,75)"
  assertEqual "Keyboard message handling" expected (handleClientMessage msg)

testMessageHandlerUnknown :: Test
testMessageHandlerUnknown = TestCase $ do
  let msg = ClientMessage "unknown" "test"
  let expected = createResponse "error" "Unknown message format. Try sending 'help' for available commands."
  assertEqual "Unknown message handling" expected (handleClientMessage msg)

-- Test suite assembly
allTests :: Test
allTests =
  TestList
    [ TestLabel "Create Welcome Response" testCreateWelcomeResponse,
      TestLabel "Create Pong Response" testCreatePongResponse,
      TestLabel "Create Echo Response" testCreateEchoResponse,
      TestLabel "Calculator Addition With Spaces" testCalculatorAdditionWithSpaces,
      TestLabel "Calculator Subtraction With Spaces" testCalculatorSubtractionWithSpaces,
      TestLabel "Calculator Multiplication With Spaces" testCalculatorMultiplicationWithSpaces,
      TestLabel "Calculator Division With Spaces" testCalculatorDivisionWithSpaces,
      TestLabel "Calculator Addition Without Spaces" testCalculatorAdditionWithoutSpaces,
      TestLabel "Calculator Subtraction Without Spaces" testCalculatorSubtractionWithoutSpaces,
      TestLabel "Calculator Multiplication Without Spaces" testCalculatorMultiplicationWithoutSpaces,
      TestLabel "Calculator Division Without Spaces" testCalculatorDivisionWithoutSpaces,
      TestLabel "Calculator Division By Zero" testCalculatorDivisionByZero,
      TestLabel "Calculator Invalid Numbers" testCalculatorInvalidNumbers,
      TestLabel "Calculator Invalid Format" testCalculatorInvalidFormat,
      TestLabel "Keyboard Handler Valid Input" testKeyboardHandlerValidInput,
      TestLabel "Keyboard Handler Invalid Input" testKeyboardHandlerInvalidInput,
      TestLabel "Keyboard Handler Missing Position" testKeyboardHandlerMissingPosition,
      TestLabel "Message Handler Ping" testMessageHandlerPing,
      TestLabel "Message Handler Echo" testMessageHandlerEcho,
      TestLabel "Message Handler Calculate" testMessageHandlerCalculate,
      TestLabel "Message Handler Help" testMessageHandlerHelp,
      TestLabel "Message Handler Keyboard" testMessageHandlerKeyboard,
      TestLabel "Message Handler Unknown" testMessageHandlerUnknown
    ]

main :: IO ()
main = do
  putStrLn "Running WebSocket Server tests..."
  putStrLn "================================="

  counts <- runTestTT allTests

  putStrLn "================================="
  putStrLn $ "Test Summary:"
  putStrLn $ "  Cases: " ++ show (cases counts)
  putStrLn $ "  Tried: " ++ show (tried counts)
  putStrLn $ "  Errors: " ++ show (errors counts)
  putStrLn $ "  Failures: " ++ show (failures counts)

  if failures counts > 0 || errors counts > 0
    then do
      putStrLn $ "ERROR: " ++ show (failures counts + errors counts) ++ " tests failed!"
      Exit.exitFailure
    else do
      putStrLn "All tests passed!"
      Exit.exitSuccess
