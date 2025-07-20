module Main where

import Control.Monad.State (runState)
import Data.List (intercalate)
import qualified GameLogic as Game
import qualified System.Exit as Exit
import System.IO (stderr)
import Test.HUnit
import Test.HUnit.Text (PutText (..), runTestText)

-- =============================================================================
-- TEST CASES FOR GAME LOGIC
-- =============================================================================

-- Test empty board initialization
testEmptyBoard :: Test
testEmptyBoard = TestCase $ do
  let board = Game.emptyBoard
  assertEqual "Empty board should have 3 rows" 3 (length board)
  assertEqual "Each row should have 3 columns" 3 (length (board !! 0))
  assertBool "All cells should be empty" (all (all (== Game.Empty)) board)

-- Test initial game state
testInitialGameState :: Test
testInitialGameState = TestCase $ do
  let state = Game.initialGameState
  assertEqual "Initial player should be X" Game.X (Game.currentPlayer state)
  assertBool "Game should not be over initially" (not (Game.gameOver state))
  assertEqual "Winner should be Nothing initially" Nothing (Game.winner state)
  assertEqual "Board should be empty" Game.emptyBoard (Game.board state)

-- Test valid moves
testValidMoves :: Test
testValidMoves = TestCase $ do
  let emptyBoardMoves = Game.validMoves Game.emptyBoard
  assertEqual "Empty board should have 9 valid moves" 9 (length emptyBoardMoves)

  let partialBoard = Game.updateBoard Game.emptyBoard (0, 0) Game.X
  let partialMoves = Game.validMoves partialBoard
  assertEqual "Board with one move should have 8 valid moves" 8 (length partialMoves)
  assertBool "Position (0,0) should not be in valid moves" (not ((0, 0) `elem` partialMoves))

-- Test board update
testUpdateBoard :: Test
testUpdateBoard = TestCase $ do
  let updatedBoard = Game.updateBoard Game.emptyBoard (1, 1) Game.X
  assertEqual "Center cell should be filled with X" (Game.Filled Game.X) (updatedBoard !! 1 !! 1)
  assertEqual "Other cells should remain empty" Game.Empty (updatedBoard !! 0 !! 0)
  assertEqual "Other cells should remain empty" Game.Empty (updatedBoard !! 2 !! 2)

-- Test winning conditions
testWinningConditions :: Test
testWinningConditions = TestCase $ do
  -- Test horizontal win
  let horizontalWin = [[Game.Filled Game.X, Game.Filled Game.X, Game.Filled Game.X], [Game.Empty, Game.Empty, Game.Empty], [Game.Empty, Game.Empty, Game.Empty]]
  assertBool "Horizontal line should be a win" (Game.hasWinningLine horizontalWin Game.X)
  assertEqual "Winner should be X" (Just Game.X) (Game.getWinner horizontalWin)

  -- Test vertical win
  let verticalWin = [[Game.Filled Game.O, Game.Empty, Game.Empty], [Game.Filled Game.O, Game.Empty, Game.Empty], [Game.Filled Game.O, Game.Empty, Game.Empty]]
  assertBool "Vertical line should be a win" (Game.hasWinningLine verticalWin Game.O)
  assertEqual "Winner should be O" (Just Game.O) (Game.getWinner verticalWin)

  -- Test diagonal win
  let diagonalWin = [[Game.Filled Game.X, Game.Empty, Game.Empty], [Game.Empty, Game.Filled Game.X, Game.Empty], [Game.Empty, Game.Empty, Game.Filled Game.X]]
  assertBool "Diagonal line should be a win" (Game.hasWinningLine diagonalWin Game.X)
  assertEqual "Winner should be X" (Just Game.X) (Game.getWinner diagonalWin)

  -- Test anti-diagonal win
  let antiDiagonalWin = [[Game.Empty, Game.Empty, Game.Filled Game.O], [Game.Empty, Game.Filled Game.O, Game.Empty], [Game.Filled Game.O, Game.Empty, Game.Empty]]
  assertBool "Anti-diagonal line should be a win" (Game.hasWinningLine antiDiagonalWin Game.O)
  assertEqual "Winner should be O" (Just Game.O) (Game.getWinner antiDiagonalWin)

-- Test game over conditions
testGameOverConditions :: Test
testGameOverConditions = TestCase $ do
  -- Test win condition
  let winBoard = [[Game.Filled Game.X, Game.Filled Game.X, Game.Filled Game.X], [Game.Empty, Game.Empty, Game.Empty], [Game.Empty, Game.Empty, Game.Empty]]
  assertBool "Game should be over when someone wins" (Game.isGameOver winBoard)

  -- Test draw condition
  let drawBoard = [[Game.Filled Game.X, Game.Filled Game.O, Game.Filled Game.X], [Game.Filled Game.O, Game.Filled Game.X, Game.Filled Game.O], [Game.Filled Game.O, Game.Filled Game.X, Game.Filled Game.O]]
  assertBool "Game should be over when board is full" (Game.isGameOver drawBoard)

  -- Test ongoing game
  let ongoingBoard = [[Game.Filled Game.X, Game.Empty, Game.Empty], [Game.Empty, Game.Empty, Game.Empty], [Game.Empty, Game.Empty, Game.Empty]]
  assertBool "Game should not be over when ongoing" (not (Game.isGameOver ongoingBoard))

-- Test State monad operations
testStateMonadOperations :: Test
testStateMonadOperations = TestCase $ do
  let initialState = Game.initialGameState
  let (boardResult, finalState) = runState Game.getBoardState initialState
  assertEqual "getBoardState should return current board" (Game.board initialState) boardResult

  let (player, _) = runState Game.getCurrentPlayerState initialState
  assertEqual "getCurrentPlayerState should return current player" (Game.currentPlayer initialState) player

-- Test make move in State monad
testMakeMoveState :: Test
testMakeMoveState = TestCase $ do
  let initialState = Game.initialGameState
  let (success, newState) = runState (Game.makeMoveState (0, 0)) initialState

  assertBool "Valid move should succeed" success
  assertEqual "Board should be updated" (Game.Filled Game.X) (Game.board newState !! 0 !! 0)
  assertEqual "Player should switch to O" Game.O (Game.currentPlayer newState)

  -- Test invalid move
  let (invalidSuccess, _) = runState (Game.makeMoveState (0, 0)) newState
  assertBool "Invalid move should fail" (not invalidSuccess)

  -- Test out of bounds move
  let (outOfBoundsSuccess, _) = runState (Game.makeMoveState (3, 3)) initialState
  assertBool "Out of bounds move should fail" (not outOfBoundsSuccess)

-- Test game engine
testGameEngine :: Test
testGameEngine = TestCase $ do
  let initialState = Game.initialGameState
  let (result, _) = runState (Game.gameEngine (Game.MakeMove (0, 0))) initialState

  case result of
    Right msg -> assertEqual "Successful move should return success message" "Move successful" msg
    Left _ -> assertFailure "Valid move should succeed"

-- Test complete game flow
testCompleteGameFlow :: Test
testCompleteGameFlow = TestCase $ do
  let moves =
        [ (0, 0),
          (1, 1),
          (0, 1),
          (2, 2),
          (0, 2)
        ] -- X wins horizontally
  let finalState = foldl (\state pos -> snd (runState (Game.makeMoveState pos) state)) Game.initialGameState moves

  assertBool "Game should be over" (Game.gameOver finalState)
  assertEqual "Winner should be X" (Just Game.X) (Game.winner finalState)

-- =============================================================================
-- CUSTOM TEST RUNNER WITH LABELS
-- =============================================================================

-- Custom test runner that shows test labels as they run
runTestsWithLabels :: Test -> IO Counts
runTestsWithLabels test = do
  putStrLn "Starting tests with labels..."

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

-- Extract test labels from a Test structure
extractTestLabels :: Test -> [String]
extractTestLabels (TestCase _) = [] -- Don't include TestCase entries
extractTestLabels (TestLabel label test) = [label] ++ extractTestLabels test
extractTestLabels (TestList tests) = concatMap extractTestLabels tests

-- Helper function to show counts
myShowCounts :: Counts -> String
myShowCounts counts =
  "Cases: "
    ++ show (cases counts)
    ++ "  Tried: "
    ++ show (tried counts)
    ++ "  Errors: "
    ++ show (errors counts)
    ++ "  Failures: "
    ++ show (failures counts)

-- =============================================================================
-- TEST SUITE ASSEMBLY
-- =============================================================================

tests :: Test
tests =
  TestList
    [ TestLabel "Empty Board" testEmptyBoard,
      TestLabel "Initial Game State" testInitialGameState,
      TestLabel "Valid Moves" testValidMoves,
      TestLabel "Board Update" testUpdateBoard,
      TestLabel "Winning Conditions" testWinningConditions,
      TestLabel "Game Over Conditions" testGameOverConditions,
      TestLabel "State Monad Operations" testStateMonadOperations,
      TestLabel "Make Move State" testMakeMoveState,
      TestLabel "Game Engine" testGameEngine,
      TestLabel "Complete Game Flow" testCompleteGameFlow
    ]

main :: IO ()
main = do
  putStrLn "Running GameLogic tests..."
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
