module Main where

import Control.Concurrent.STM (STM, atomically)
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified GameLogic as Game
import Server
import qualified System.Exit as Exit
import Test.HUnit

-- =============================================================================
-- TEST CASES FOR SERVER LOGIC
-- =============================================================================

-- | Test board conversion utilities
-- |
-- | This test validates the conversion functions between the internal Game.Board type
-- | and the JSON-friendly string representation used in HTTP responses.
-- |
-- | Tests:
-- | - Empty board conversion (round-trip)
-- | - Partial board conversion (board with one move)
-- |
-- | Ensures that board state is preserved correctly when converting between
-- | internal representation and JSON format for API communication.
testBoardConversion :: Test
testBoardConversion = TestCase $ do
  let emptyBoard = Game.emptyBoard
  let jsonBoard = boardToJson emptyBoard
  let convertedBack = jsonToBoard jsonBoard

  assertEqual "Empty board should convert correctly" emptyBoard convertedBack

  let partialBoard = Game.updateBoard emptyBoard (0, 0) Game.X
  let partialJson = boardToJson partialBoard
  let partialConverted = jsonToBoard partialJson

  assertEqual "Partial board should convert correctly" partialBoard partialConverted

-- | Test initial server state
-- |
-- | This test verifies that the server's initial state correctly wraps the
-- | game logic's initial state.
-- |
-- | Ensures that:
-- | - Server state contains the correct GameState
-- | - Initial state matches GameLogic.initialGameState
-- | - Server properly integrates with the underlying game logic
testInitialServerState :: Test
testInitialServerState = TestCase $ do
  let state = initialServerState
  assertEqual
    "Initial game state should match GameLogic initial state"
    Game.initialGameState
    (gameState state)

-- | Test JSON serialization/deserialization
-- |
-- | This test validates that the JSON API types (GameRequest and GameResponse)
-- | can be properly serialized to JSON and deserialized back to Haskell types.
-- |
-- | Tests:
-- | - GameRequest serialization/deserialization
-- | - GameResponse serialization/deserialization
-- |
-- | Ensures that the HTTP API can properly communicate with clients by
-- | maintaining data integrity through JSON transformations.
testJsonSerialization :: Test
testJsonSerialization = TestCase $ do
  let request = GameRequest {action = "new_game", position = Nothing}
  let encoded = Aeson.encode request
  let decoded = Aeson.decode encoded :: Maybe GameRequest

  assertBool "Request should serialize and deserialize correctly" (Just request == decoded)

  let response =
        GameResponse
          { board = [[" ", " ", " "], [" ", " ", " "], [" ", " ", " "]],
            currentPlayer = "X",
            gameOver = False,
            winner = Nothing,
            message = "Test message",
            validMoves = [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
          }
  let encodedResp = Aeson.encode response
  let decodedResp = Aeson.decode encodedResp :: Maybe GameResponse

  assertBool "Response should serialize and deserialize correctly" (Just response == decodedResp)

-- | Test response generation
-- |
-- | This test verifies that the server correctly generates GameResponse objects
-- | from the current server state.
-- |
-- | Ensures that:
-- | - Board is correctly converted to JSON format
-- | - Current player is properly serialized
-- | - Game over status is accurate
-- | - Winner information is correctly handled
-- | - Valid moves list is properly generated
-- |
-- | This is critical for ensuring the API provides accurate game state to clients.
testResponseGeneration :: Test
testResponseGeneration = TestCase $ do
  let state = initialServerState
  let response = getStateResponse state

  assertEqual
    "Response should have correct board"
    (boardToJson (Game.board (gameState state)))
    (board response)
  assertEqual
    "Response should have correct current player"
    (show (Game.currentPlayer (gameState state)))
    (currentPlayer response)
  assertEqual
    "Response should have correct game over status"
    (Game.gameOver (gameState state))
    (gameOver response)
  assertEqual
    "Response should have correct winner"
    (fmap show (Game.winner (gameState state)))
    (winner response)
  assertEqual
    "Response should have correct valid moves"
    (Game.validMoves (Game.board (gameState state)))
    (validMoves response)

-- | Test error response generation
-- |
-- | This test validates that the server correctly generates error responses
-- | when something goes wrong.
-- |
-- | Ensures that error responses have:
-- | - Correct error message
-- | - Empty board (reset state)
-- | - Default current player (X)
-- | - Game not over
-- | - No winner
-- | - No valid moves
-- |
-- | This is important for providing meaningful error feedback to clients.
testErrorResponse :: Test
testErrorResponse = TestCase $ do
  let errorMsg = "Test error message"
  let response = errorResponse errorMsg

  assertEqual "Error response should have correct message" errorMsg (message response)
  assertEqual
    "Error response should have empty board"
    (boardToJson Game.emptyBoard)
    (board response)
  assertEqual "Error response should have X as current player" "X" (currentPlayer response)
  assertBool "Error response should not be game over" (not (gameOver response))
  assertEqual "Error response should have no winner" Nothing (winner response)
  assertEqual "Error response should have no valid moves" [] (validMoves response)

-- | Test new game action
-- |
-- | This test verifies that the "new_game" action correctly resets the game
-- | to its initial state.
-- |
-- | Ensures that:
-- | - Server state is reset to initial game state
-- | - Response contains correct "New game started" message
-- | - Board is reset to empty state
-- |
-- | This is essential for allowing players to start fresh games.
testNewGameAction :: Test
testNewGameAction = TestCase $ do
  let request = GameRequest {action = "new_game", position = Nothing}
  let state = initialServerState
  (response, newState) <- runSTM (processGameActionSTM request state)

  assertEqual
    "New game should reset to initial state"
    Game.initialGameState
    (gameState newState)
  assertEqual
    "New game response should have correct message"
    "New game started"
    (message response)
  assertEqual
    "New game response should have correct board"
    (boardToJson Game.emptyBoard)
    (board response)

-- | Test get state action
-- |
-- | This test verifies that the "get_state" action returns the current
-- | game state without modifying it.
-- |
-- | Ensures that:
-- | - Server state remains unchanged
-- | - Response contains correct "Current game state" message
-- | - Board information is accurately reflected
-- |
-- | This allows clients to poll the server for current game status.
testGetStateAction :: Test
testGetStateAction = TestCase $ do
  let request = GameRequest {action = "get_state", position = Nothing}
  let state = initialServerState
  (response, newState) <- runSTM (processGameActionSTM request state)

  assertEqual "Get state should not change server state" state newState
  assertEqual
    "Get state response should have correct message"
    "Current game state"
    (message response)
  assertEqual
    "Get state response should have correct board"
    (boardToJson (Game.board (gameState state)))
    (board response)

-- | Test make move action - valid move
-- |
-- | This test verifies that valid moves are properly processed and applied
-- | to the game state.
-- |
-- | Ensures that:
-- | - Board is updated with the move
-- | - Current player switches (X -> O or O -> X)
-- | - Response contains appropriate message
-- |
-- | This is the core functionality for playing the game.
testMakeMoveActionValid :: Test
testMakeMoveActionValid = TestCase $ do
  let request = GameRequest {action = "make_move", position = Just (0, 0)}
  let state = initialServerState
  (response, newState) <- runSTM (processGameActionSTM request state)

  assertEqual
    "Valid move should update board"
    (Game.Filled Game.X)
    (Game.board (gameState newState) !! 0 !! 0)
  assertEqual
    "Valid move should switch player"
    Game.O
    (Game.currentPlayer (gameState newState))
  assertEqual
    "Valid move response should have correct message"
    "Current game state"
    (message response)

-- | Test make move action - invalid move
-- |
-- | This test verifies that invalid moves (out of bounds) are properly
-- | rejected without changing the game state.
-- |
-- | Ensures that:
-- | - Server state remains unchanged
-- | - Error message is returned
-- | - Game continues normally
-- |
-- | This prevents invalid moves from corrupting the game state.
testMakeMoveActionInvalid :: Test
testMakeMoveActionInvalid = TestCase $ do
  let request = GameRequest {action = "make_move", position = Just (3, 3)}
  let state = initialServerState
  (response, newState) <- runSTM (processGameActionSTM request state)

  assertEqual "Invalid move should not change server state" state newState
  assertEqual
    "Invalid move response should have error message"
    "Invalid move"
    (message response)

-- | Test make move action - missing position
-- |
-- | This test verifies that make_move requests without a position are
-- | properly handled as errors.
-- |
-- | Ensures that:
-- | - Server state remains unchanged
-- | - Appropriate error message is returned
-- | - Game continues normally
-- |
-- | This validates proper API usage and prevents malformed requests.
testMakeMoveActionMissingPosition = TestCase $ do
  let request = GameRequest {action = "make_move", position = Nothing}
  let state = initialServerState
  (response, newState) <- runSTM (processGameActionSTM request state)

  assertEqual "Missing position should not change server state" state newState
  assertEqual
    "Missing position response should have error message"
    "Position required for make_move"
    (message response)

-- | Test unknown action
-- |
-- | This test verifies that unknown or unsupported actions are properly
-- | handled as errors.
-- |
-- | Ensures that:
-- | - Server state remains unchanged
-- | - Error message is returned
-- | - Game continues normally
-- |
-- | This provides graceful handling of malformed or unsupported API requests.
testUnknownAction :: Test
testUnknownAction = TestCase $ do
  let request = GameRequest {action = "unknown_action", position = Nothing}
  let state = initialServerState
  (response, newState) <- runSTM (processGameActionSTM request state)

  assertEqual "Unknown action should not change server state" state newState
  assertEqual
    "Unknown action response should have error message"
    "Unknown action"
    (message response)

-- | Test complete game flow through server actions
-- |
-- | This test simulates a complete game from start to finish using
-- | server actions, ending with a winning condition.
-- |
-- | Tests a sequence of moves that results in X winning:
-- | - X plays at (0,0), O plays at (1,1)
-- | - X plays at (0,1), O plays at (2,2)
-- | - X plays at (0,2) to win
-- |
-- | Ensures that:
-- | - Multiple moves can be made sequentially
-- | - Game state progresses correctly
-- | - Winning conditions are properly detected
-- | - Game over state is reached
-- |
-- | This validates the complete game flow through the server API.
testCompleteGameFlow :: Test
testCompleteGameFlow = TestCase $ do
  let state = initialServerState

  -- Make moves to create a winning scenario for X
  let moves = [(0, 0), (1, 1), (0, 1), (2, 2), (0, 2)]
  finalState <-
    foldM
      ( \s pos -> do
          (_, newState) <-
            runSTM
              ( processGameActionSTM
                  (GameRequest {action = "make_move", position = Just pos})
                  s
              )
          return newState
      )
      state
      moves

  assertBool
    "Game should be over after winning moves"
    (Game.gameOver (gameState finalState))
  assertEqual
    "Winner should be X"
    (Just Game.X)
    (Game.winner (gameState finalState))

-- =============================================================================
-- HELPER FUNCTIONS FOR TESTING
-- =============================================================================

-- Helper function to run STM actions in tests
runSTM :: STM a -> IO a
runSTM = atomically

-- =============================================================================
-- CUSTOM TEST RUNNER WITH LABELS
-- =============================================================================

-- Custom test runner that shows test labels as they run
runTestsWithLabels :: Test -> IO Counts
runTestsWithLabels test = do
  putStrLn "Starting server tests with labels..."

  -- Extract and run each test individually
  let labeledTests = extractLabeledTests test
  putStrLn $ "Found " ++ show (length labeledTests) ++ " tests:"

  -- Run each test individually and show results
  results <- mapM runSingleLabeledTest labeledTests
  let totalFailures = sum [if passed then 0 else 1 | (_, passed) <- results]
  let totalErrors = sum [if passed then 0 else 1 | (_, passed) <- results] -- Simplified for now
  putStrLn $
    "\nFinal result: Cases: "
      ++ show (length labeledTests)
      ++ "  Tried: "
      ++ show (length labeledTests)
      ++ "  Errors: "
      ++ show totalErrors
      ++ "  Failures: "
      ++ show totalFailures

  return
    Counts
      { cases = length labeledTests,
        tried = length labeledTests,
        errors = totalErrors,
        failures = totalFailures
      }

-- Extract labeled tests from a Test structure
extractLabeledTests :: Test -> [(String, Test)]
extractLabeledTests (TestCase _) = []
extractLabeledTests (TestLabel label test) = [(label, test)]
extractLabeledTests (TestList tests) = concatMap extractLabeledTests tests

-- Run a single labeled test and show the result
runSingleLabeledTest :: (String, Test) -> IO (String, Bool)
runSingleLabeledTest (label, test) = do
  putStrLn $ "Running: " ++ label
  counts <- runTestTT test
  let passed = failures counts == 0 && errors counts == 0
  putStrLn $ "  " ++ label ++ ": " ++ if passed then "PASS" else "FAIL"
  return (label, passed)

-- =============================================================================
-- TEST SUITE ASSEMBLY
-- =============================================================================

tests :: Test
tests =
  TestList
    [ TestLabel "Board Conversion" testBoardConversion,
      TestLabel "Initial Server State" testInitialServerState,
      TestLabel "JSON Serialization" testJsonSerialization,
      TestLabel "Response Generation" testResponseGeneration,
      TestLabel "Error Response" testErrorResponse,
      TestLabel "New Game Action" testNewGameAction,
      TestLabel "Get State Action" testGetStateAction,
      TestLabel "Make Move Action Valid" testMakeMoveActionValid,
      TestLabel "Make Move Action Invalid" testMakeMoveActionInvalid,
      TestLabel "Make Move Action Missing Position" testMakeMoveActionMissingPosition,
      TestLabel "Unknown Action" testUnknownAction,
      TestLabel "Complete Game Flow" testCompleteGameFlow
    ]

main :: IO ()
main = do
  putStrLn "Running Server tests..."
  putStrLn "========================================"

  -- Use our custom test runner
  counts <- runTestsWithLabels tests

  putStrLn "========================================"
  putStrLn $ "Test Summary:"
  putStrLn $ "  Cases: " ++ show (cases counts)
  putStrLn $ "  Tried: " ++ show (tried counts)
  putStrLn $ "  Errors: " ++ show (errors counts)
  putStrLn $ "  Failures: " ++ show (failures counts)

  if failures counts > 0 || errors counts > 0 then Exit.exitFailure else Exit.exitSuccess
