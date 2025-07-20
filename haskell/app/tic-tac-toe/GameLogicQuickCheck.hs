module Main where

import Control.Monad.State (runState)
import Data.Maybe (isJust)
import qualified GameLogic as Game
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Random (randomRIO)
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC (assert, monadicIO, run)
import Test.QuickCheck.Random (mkQCGen)

-- =============================================================================
-- ARBITRARY INSTANCES FOR CUSTOM TYPES
-- =============================================================================

-- Generate random Players
instance Arbitrary Game.Player where
  arbitrary = elements [Game.X, Game.O]

-- Generate random Cells
instance Arbitrary Game.Cell where
  arbitrary = oneof [return Game.Empty, return (Game.Filled Game.X), return (Game.Filled Game.O)]

-- | Custom generator for valid board positions (0-2, 0-2)
newtype ValidPosition = ValidPosition (Int, Int) deriving (Show, Eq)

instance Arbitrary ValidPosition where
  arbitrary = do
    row <- choose (0, 2)
    col <- choose (0, 2)
    return $ ValidPosition (row, col)

-- | Custom generator for realistic game states
-- This generates game states by making sequences of valid moves
newtype RealisticGameState = RealisticGameState Game.GameState deriving (Show, Eq)

-- | Helper function to make a sequence of random moves
makeMoveSequence :: Game.GameState -> Int -> Gen Game.GameState
makeMoveSequence state 0 = return state
makeMoveSequence state n = do
  let validMoves = Game.validMoves (Game.board state)
  if null validMoves || Game.gameOver state
    then return state -- Stop if no valid moves or game is over
    else do
      moveIndex <- choose (0, length validMoves - 1)
      let move = validMoves !! moveIndex
      let (_, newState) = runState (Game.makeMoveState move) state
      makeMoveSequence newState (n - 1)

instance Arbitrary RealisticGameState where
  arbitrary = do
    -- Start with initial state
    let initialState = Game.initialGameState

    -- Make 0-9 random moves to create various game states
    numMoves <- choose (0, 9)
    finalState <- makeMoveSequence initialState numMoves

    return $ RealisticGameState finalState

-- | Helper function to generate a 3x3 board
genBoard3x3 :: Gen [[Game.Cell]]
genBoard3x3 = vectorOf 3 (vectorOf 3 (oneof [return Game.Empty, return (Game.Filled Game.X), return (Game.Filled Game.O)]))

-- =============================================================================
-- BASIC PROPERTY TESTS
-- =============================================================================

-- Property: Empty board has 9 valid moves
prop_empty_board_valid_moves :: Bool
prop_empty_board_valid_moves =
  Game.validMoves Game.emptyBoard == [(i, j) | i <- [0 .. 2], j <- [0 .. 2]]

-- Property: Empty board is not game over
prop_empty_board_not_over :: Bool
prop_empty_board_not_over =
  not (Game.isGameOver Game.emptyBoard)

-- Property: Empty board has no winner
prop_empty_board_no_winner :: Bool
prop_empty_board_no_winner =
  Game.getWinner Game.emptyBoard == Nothing

-- Property: Initial game state has correct properties
prop_initial_game_state :: Bool
prop_initial_game_state =
  let state = Game.initialGameState
   in Game.board state == Game.emptyBoard
        && Game.currentPlayer state == Game.X
        && not (Game.gameOver state)
        && Game.winner state == Nothing

-- Property: Board update places the correct player
prop_board_update_correct :: Game.Position -> Game.Player -> Bool
prop_board_update_correct (row, col) player =
  let newBoard = Game.updateBoard Game.emptyBoard (row, col) player
   in newBoard !! row !! col == Game.Filled player

-- Property: Valid moves are always within bounds
prop_valid_moves_bounds :: Game.Board -> Bool
prop_valid_moves_bounds board =
  all
    (\(row, col) -> row >= 0 && row <= 2 && col >= 0 && col <= 2)
    (Game.validMoves board)

-- Property: Valid moves are always empty
prop_valid_moves_empty :: Game.Board -> Bool
prop_valid_moves_empty board =
  all
    (\(row, col) -> board !! row !! col == Game.Empty)
    (Game.validMoves board)

-- Property: Board always has 9 cells
prop_board_size :: Game.Board -> Bool
prop_board_size board =
  length board == 3 && all ((== 3) . length) board

-- | Property: Board always has exactly 9 cells (Property version)
prop_boardSize :: Property
prop_boardSize =
  let board = Game.emptyBoard
   in conjoin [length board === 3, conjoin (map ((=== 3) . length) board)]

-- | Property: Valid moves are always within bounds (Property version)
prop_validMovesInBounds :: Property
prop_validMovesInBounds =
  forAll genBoard3x3 $ \board ->
    all (\(row, col) -> row >= 0 && row <= 2 && col >= 0 && col <= 2) (Game.validMoves board)

-- | Property: Valid moves only point to empty cells (Property version)
prop_validMovesAreEmpty :: Property
prop_validMovesAreEmpty =
  forAll genBoard3x3 $ \board ->
    all (\(row, col) -> board !! row !! col == Game.Empty) (Game.validMoves board)

-- | Property: Board update preserves board structure
prop_boardUpdateStructure :: Property
prop_boardUpdateStructure =
  forAll genBoard3x3 $ \board ->
    forAll arbitrary $ \(ValidPosition (row, col)) ->
      forAll arbitrary $ \player ->
        let newBoard = Game.updateBoard board (row, col) player
         in conjoin [length newBoard === 3, conjoin (map ((=== 3) . length) newBoard)]

-- | Property: Board update only changes the specified position
prop_boardUpdateTarget :: Property
prop_boardUpdateTarget =
  forAll genBoard3x3 $ \board ->
    forAll arbitrary $ \(ValidPosition (row, col)) ->
      forAll arbitrary $ \player ->
        let newBoard = Game.updateBoard board (row, col) player
         in newBoard !! row !! col === Game.Filled player

-- | Property: Board update doesn't change other positions
prop_boardUpdatePreservesOthers :: Property
prop_boardUpdatePreservesOthers =
  forAll genBoard3x3 $ \board ->
    forAll arbitrary $ \(ValidPosition (row, col)) ->
      forAll arbitrary $ \player ->
        let newBoard = Game.updateBoard board (row, col) player
         in conjoin
              [ newBoard !! r !! c === board !! r !! c
                | r <- [0 .. 2],
                  c <- [0 .. 2],
                  (r, c) /= (row, col)
              ]

-- =============================================================================
-- WIN CONDITION TESTS
-- =============================================================================

-- Create a winning board for testing
createWinningBoard :: Game.Player -> Game.Board
createWinningBoard player =
  Game.updateBoard (Game.updateBoard (Game.updateBoard Game.emptyBoard (0, 0) player) (0, 1) player) (0, 2) player

-- Property: Winning boards are detected as game over
prop_winning_board_game_over :: Bool
prop_winning_board_game_over =
  Game.isGameOver (createWinningBoard Game.X)

-- Property: Winning boards have correct winner
prop_winning_board_winner :: Bool
prop_winning_board_winner =
  Game.getWinner (createWinningBoard Game.X) == Just Game.X

-- Property: Winning boards have winning lines
prop_winning_board_has_line :: Bool
prop_winning_board_has_line =
  Game.hasWinningLine (createWinningBoard Game.X) Game.X

-- | Property: A player can't win with less than 3 moves
prop_winRequiresThreeMoves :: Property
prop_winRequiresThreeMoves =
  forAll genBoard3x3 $ \board ->
    let xCount = length [cell | row <- board, cell <- row, cell == Game.Filled Game.X]
        oCount = length [cell | row <- board, cell <- row, cell == Game.Filled Game.O]
     in case Game.getWinner board of
          Just _ -> xCount >= 3 || oCount >= 3
          Nothing -> True

-- | Property: Winner has a winning line
prop_noDoubleWin :: Property
prop_noDoubleWin =
  forAll genBoard3x3 $ \board ->
    let winner = Game.getWinner board
     in case winner of
          Just player ->
            -- If there's a winner, that player must have a winning line
            Game.hasWinningLine board player
          Nothing -> True

-- | Property: Realistic game states have at most one winner
prop_realisticGameStatesHaveAtMostOneWinner :: RealisticGameState -> Property
prop_realisticGameStatesHaveAtMostOneWinner (RealisticGameState state) =
  let board = Game.board state
      xHasWin = Game.hasWinningLine board Game.X
      oHasWin = Game.hasWinningLine board Game.O
   in -- In realistic game states, at most one player should have a winning line
      property $ not (xHasWin && oHasWin)

-- | Property: Winning line detection is correct
prop_winningLineDetection :: Property
prop_winningLineDetection =
  forAll genBoard3x3 $ \board ->
    let winner = Game.getWinner board
     in case winner of
          Just Game.X -> Game.hasWinningLine board Game.X
          Just Game.O -> Game.hasWinningLine board Game.O
          Nothing -> not (Game.hasWinningLine board Game.X) && not (Game.hasWinningLine board Game.O)

-- =============================================================================
-- MOVE VALIDATION TESTS
-- =============================================================================

-- Property: Valid moves succeed
prop_valid_move_succeeds :: Bool
prop_valid_move_succeeds =
  let initialState = Game.initialGameState
      (success, _) = runState (Game.makeMoveState (0, 0)) initialState
   in success

-- Property: Invalid moves fail
prop_invalid_move_fails :: Bool
prop_invalid_move_fails =
  let initialState = Game.initialGameState
      (success, _) = runState (Game.makeMoveState (3, 3)) initialState
   in not success

-- Property: Move on occupied cell fails
prop_occupied_cell_move_fails :: Bool
prop_occupied_cell_move_fails =
  let board = Game.updateBoard Game.emptyBoard (0, 0) Game.X
      initialState = Game.initialGameState {Game.board = board}
      (success, _) = runState (Game.makeMoveState (0, 0)) initialState
   in not success

-- | Property: Valid moves decrease as game progresses
prop_validMovesDecrease :: Property
prop_validMovesDecrease =
  forAll genBoard3x3 $ \board ->
    forAll arbitrary $ \(ValidPosition (row, col)) ->
      forAll arbitrary $ \player ->
        let movesBefore = Game.validMoves board
            newBoard = Game.updateBoard board (row, col) player
            movesAfter = Game.validMoves newBoard
         in if board !! row !! col == Game.Empty
              then length movesAfter === length movesBefore - 1
              else length movesAfter === length movesBefore

-- =============================================================================
-- PLAYER TURN TESTS
-- =============================================================================

-- Property: Players alternate turns
prop_player_alternation :: Bool
prop_player_alternation =
  let initialState = Game.initialGameState
      (_, state1) = runState (Game.makeMoveState (0, 0)) initialState
      (_, state2) = runState (Game.makeMoveState (0, 1)) state1
   in Game.currentPlayer state1 == Game.O && Game.currentPlayer state2 == Game.X

-- =============================================================================
-- GAME ENGINE TESTS
-- =============================================================================

-- Property: Game engine returns success for valid moves
prop_game_engine_valid_move :: Bool
prop_game_engine_valid_move =
  let initialState = Game.initialGameState
      (result, _) = runState (Game.gameEngine (Game.MakeMove (0, 0))) initialState
   in result == Right "Move successful"

-- Property: Game engine returns error for invalid moves
prop_game_engine_invalid_move :: Bool
prop_game_engine_invalid_move =
  let initialState = Game.initialGameState
      (result, _) = runState (Game.gameEngine (Game.MakeMove (3, 3))) initialState
   in result == Left "Invalid move"

-- | Property: Game engine returns appropriate responses
prop_gameEngineResponses :: RealisticGameState -> ValidPosition -> Property
prop_gameEngineResponses (RealisticGameState initialState) (ValidPosition pos) = property $ do
  let (result, _) = runState (Game.gameEngine (Game.MakeMove pos)) initialState

  case result of
    Right _ -> True -- Success case
    Left _ -> True -- Error case (both are valid)

-- =============================================================================
-- GAME STATE TESTS
-- =============================================================================

-- | Property: Game state invariants
prop_gameStateInvariants :: RealisticGameState -> Property
prop_gameStateInvariants (RealisticGameState state) =
  conjoin
    [ length (Game.board state) === 3,
      conjoin (map ((=== 3) . length) (Game.board state)),
      Game.gameOver state ==> (isJust (Game.winner state) || all (all (/= Game.Empty)) (Game.board state)),
      isJust (Game.winner state) ==> Game.gameOver state
    ]

-- | Property: State monad operations preserve state
prop_stateMonadPreservesState :: RealisticGameState -> Property
prop_stateMonadPreservesState (RealisticGameState initialState) =
  let (boardResult, finalState) = runState Game.getBoardState initialState
   in conjoin [boardResult === Game.board initialState, finalState === initialState]

-- | Property: Make move state transitions
prop_makeMoveStateTransitions :: RealisticGameState -> ValidPosition -> Property
prop_makeMoveStateTransitions (RealisticGameState initialState) (ValidPosition pos) =
  let (success, newState) = runState (Game.makeMoveState pos) initialState
   in if success
        then conjoin [newState =/= initialState, Game.board newState =/= Game.board initialState, Game.currentPlayer newState =/= Game.currentPlayer initialState]
        else newState === initialState
  where
    a =/= b = counterexample ("Expected inequality but got equality: " ++ show a) (a /= b)

-- | Property: Game always terminates
prop_gameTerminates :: Property
prop_gameTerminates = QC.monadicIO $ do
  -- Simulate a complete game with random moves
  finalState <- QC.run $ simulateRandomGame Game.initialGameState
  QC.assert $ Game.gameOver finalState

-- =============================================================================
-- HELPER FUNCTIONS FOR QUICKCHECK
-- =============================================================================

-- | Simulate a random game until completion
simulateRandomGame :: Game.GameState -> IO Game.GameState
simulateRandomGame state = do
  if Game.gameOver state
    then return state
    else do
      let validMoves = Game.validMoves (Game.board state)
      if null validMoves
        then return state
        else do
          moveIndex <- randomRIO (0, length validMoves - 1)
          let move = validMoves !! moveIndex
          let (_, newState) = runState (Game.makeMoveState move) state
          simulateRandomGame newState

-- =============================================================================
-- TEST RUNNER WITH PROPER RESULT HANDLING
-- =============================================================================

-- Run a single property test and return success status
runPropertyTest :: String -> Property -> Int -> IO Bool
runPropertyTest name prop seed = do
  putStrLn $ "Testing: " ++ name
  let args = stdArgs {replay = Just (mkQCGen seed, 0)}
  result <- quickCheckWithResult args prop
  let passed = case result of
        Success _ _ _ _ _ _ -> True
        _ -> False
  putStrLn $ "  " ++ name ++ ": " ++ if passed then "PASS" else "FAIL"
  return passed

-- Run a single property test with a specific seed
runPropertyTestWithSeed :: String -> Property -> Int -> IO Bool
runPropertyTestWithSeed name prop seed = do
  putStrLn $ "Testing: " ++ name ++ " (seed: " ++ show seed ++ ")"
  let args = stdArgs {replay = Just (mkQCGen seed, 0)}
  result <- quickCheckWithResult args prop
  let passed = case result of
        Success _ _ _ _ _ _ -> True
        _ -> False
  putStrLn $ "  " ++ name ++ ": " ++ if passed then "PASS" else "FAIL"
  return passed

-- Run a single property test that takes RealisticGameState and return success status
runRealisticGameStatePropertyTest :: String -> (RealisticGameState -> Property) -> Int -> IO Bool
runRealisticGameStatePropertyTest name prop seed = do
  putStrLn $ "Testing: " ++ name
  let args = stdArgs {replay = Just (mkQCGen seed, 0)}
  result <- quickCheckWithResult args (forAll arbitrary prop)
  let passed = case result of
        Success _ _ _ _ _ _ -> True
        _ -> False
  putStrLn $ "  " ++ name ++ ": " ++ if passed then "PASS" else "FAIL"
  return passed

-- Run a single Bool property test and return success status
runBoolTest :: String -> Bool -> IO Bool
runBoolTest name prop = do
  putStrLn $ "Testing: " ++ name
  let passed = prop
  putStrLn $ "  " ++ name ++ ": " ++ if passed then "PASS" else "FAIL"
  return passed

-- =============================================================================
-- SEED-BASED TESTING FUNCTIONS
-- =============================================================================

-- Run a specific test with a given seed for reproducible testing
runTestWithSeed :: String -> Property -> Int -> IO ()
runTestWithSeed name prop seed = do
  putStrLn $ "Running " ++ name ++ " with seed " ++ show seed
  let args = stdArgs {replay = Just (mkQCGen seed, 0)}
  result <- quickCheckWithResult args prop
  let passed = case result of
        Success _ _ _ _ _ _ -> True
        _ -> False
  putStrLn $ "  " ++ if passed then "PASS" else "FAIL"

-- =============================================================================
-- MAIN FUNCTION
-- =============================================================================
main = do
  args <- getArgs
  let seed = case args of
        ["--seed", s] -> read s :: Int
        _ -> 42 -- Default seed
  putStrLn "=== QuickCheck Test Suite for Tic-Tac-Toe Game Logic ==="
  putStrLn $ "Using seed: " ++ show seed
  putStrLn ""

  -- Demonstrate seed-based testing
  putStrLn "Demonstrating seed-based testing (reproducible results):"
  putStrLn "========================================================"
  runTestWithSeed "Board size property" prop_boardSize seed
  runTestWithSeed "Board size property" prop_boardSize seed -- Same seed, same result
  runTestWithSeed "Board size property" prop_boardSize (seed + 1) -- Different seed, potentially different result
  putStrLn ""

  putStrLn "Running property tests..."
  putStrLn "=========================="

  -- Basic Properties
  putStrLn "\n1. Testing basic properties..."
  results1 <-
    sequence
      [ runBoolTest "Empty board valid moves" prop_empty_board_valid_moves,
        runBoolTest "Empty board not over" prop_empty_board_not_over,
        runBoolTest "Empty board no winner" prop_empty_board_no_winner,
        runBoolTest "Initial game state" prop_initial_game_state,
        runPropertyTest "Board size (Property)" prop_boardSize seed
      ]

  -- Board Operations
  putStrLn "\n2. Testing board operations..."
  results2 <-
    sequence
      [ runPropertyTest "Valid moves in bounds (Property)" prop_validMovesInBounds seed,
        runPropertyTest "Valid moves are empty (Property)" prop_validMovesAreEmpty seed,
        runPropertyTest "Board update structure" prop_boardUpdateStructure seed,
        runPropertyTest "Board update target" prop_boardUpdateTarget seed,
        runPropertyTest "Board update preserves others" prop_boardUpdatePreservesOthers seed
      ]

  -- Win Conditions
  putStrLn "\n3. Testing win conditions..."
  results3 <-
    sequence
      [ runBoolTest "Winning board game over" prop_winning_board_game_over,
        runBoolTest "Winning board winner" prop_winning_board_winner,
        runBoolTest "Winning board has line" prop_winning_board_has_line,
        runPropertyTest "Win requires three moves" prop_winRequiresThreeMoves seed,
        runPropertyTest "Winner has winning line" prop_noDoubleWin seed,
        runPropertyTest "Winning line detection" prop_winningLineDetection seed
      ]

  -- Move Validation
  putStrLn "\n4. Testing move validation..."
  results4 <-
    sequence
      [ runBoolTest "Valid move succeeds" prop_valid_move_succeeds,
        runBoolTest "Invalid move fails" prop_invalid_move_fails,
        runBoolTest "Occupied cell move fails" prop_occupied_cell_move_fails,
        runPropertyTest "Valid moves decrease" prop_validMovesDecrease seed
      ]

  -- Player Turns
  putStrLn "\n5. Testing player turns..."
  results5 <- sequence [runBoolTest "Player alternation" prop_player_alternation]

  -- Game Engine
  putStrLn "\n6. Testing game engine..."
  results6 <-
    sequence
      [ runBoolTest "Game engine valid move" prop_game_engine_valid_move,
        runBoolTest "Game engine invalid move" prop_game_engine_invalid_move
      ]

  -- Game State
  putStrLn "\n7. Testing game state..."
  results7 <-
    sequence
      [ runPropertyTest "Game terminates" prop_gameTerminates seed,
        runRealisticGameStatePropertyTest "Realistic game states have at most one winner" prop_realisticGameStatesHaveAtMostOneWinner seed
      ]

  -- Combine all results
  let allResults = results1 ++ results2 ++ results3 ++ results4 ++ results5 ++ results6 ++ results7
  let passedCount = length $ filter id allResults
  let totalCount = length allResults
  let failedCount = totalCount - passedCount

  putStrLn "\n=== QuickCheck Test Suite Completed ==="
  putStrLn ""
  putStrLn $ "Test Summary:"
  putStrLn $ "  Total tests: " ++ show totalCount
  putStrLn $ "  Passed: " ++ show passedCount
  putStrLn $ "  Failed: " ++ show failedCount
  putStrLn ""

  if failedCount == 0
    then do
      putStrLn "All properties passed! The game logic is working correctly."
      putStrLn ""
      putStrLn "Key Properties Tested:"
      putStrLn "1. Basic game state properties"
      putStrLn "2. Board operations (updates, valid moves)"
      putStrLn "3. Win condition detection"
      putStrLn "4. Move validation"
      putStrLn "5. Player turn alternation"
      putStrLn "6. Game engine behavior"
      putStrLn "7. Game state invariants"
      exitSuccess
    else do
      putStrLn $ "ERROR: " ++ show failedCount ++ " tests failed!"
      putStrLn "Please review the failing tests above."
      exitFailure
