# Server Test Documentation

This document provides a comprehensive overview of all tests in the Tic-Tac-Toe server test suite, explaining what each test validates and why it's important.

## Test Categories

The server tests are organized into several categories:

1. **Data Conversion Tests** - Validate data transformation between internal and external formats
2. **State Management Tests** - Verify server state initialization and management
3. **API Communication Tests** - Test JSON serialization/deserialization
4. **Response Generation Tests** - Validate server response creation
5. **Action Processing Tests** - Test individual game actions
6. **Error Handling Tests** - Verify proper error responses
7. **Integration Tests** - Test complete game flows

---

## 1. Data Conversion Tests

### `testBoardConversion`
**Purpose**: Validates the conversion functions between the internal `Game.Board` type and the JSON-friendly string representation used in HTTP responses.

**What it tests**:
- Empty board conversion (round-trip)
- Partial board conversion (board with one move)

**Why it's important**: Ensures that board state is preserved correctly when converting between internal representation and JSON format for API communication. This is critical for maintaining data integrity between the server and client.

---

## 2. State Management Tests

### `testInitialServerState`
**Purpose**: Verifies that the server's initial state correctly wraps the game logic's initial state.

**What it tests**:
- Server state contains the correct `GameState`
- Initial state matches `GameLogic.initialGameState`
- Server properly integrates with the underlying game logic

**Why it's important**: Ensures the server correctly initializes and integrates with the game logic module. This is the foundation for all other server operations.

---

## 3. API Communication Tests

### `testJsonSerialization`
**Purpose**: Validates that the JSON API types (`GameRequest` and `GameResponse`) can be properly serialized to JSON and deserialized back to Haskell types.

**What it tests**:
- `GameRequest` serialization/deserialization
- `GameResponse` serialization/deserialization

**Why it's important**: Ensures that the HTTP API can properly communicate with clients by maintaining data integrity through JSON transformations. This is essential for client-server communication.

---

## 4. Response Generation Tests

### `testResponseGeneration`
**Purpose**: Verifies that the server correctly generates `GameResponse` objects from the current server state.

**What it tests**:
- Board is correctly converted to JSON format
- Current player is properly serialized
- Game over status is accurate
- Winner information is correctly handled
- Valid moves list is properly generated

**Why it's important**: This is critical for ensuring the API provides accurate game state to clients. Clients depend on this information to display the game correctly.

### `testErrorResponse`
**Purpose**: Validates that the server correctly generates error responses when something goes wrong.

**What it tests**:
- Correct error message
- Empty board (reset state)
- Default current player (X)
- Game not over
- No winner
- No valid moves

**Why it's important**: Provides meaningful error feedback to clients and prevents invalid states from propagating.

---

## 5. Action Processing Tests

### `testNewGameAction`
**Purpose**: Verifies that the "new_game" action correctly resets the game to its initial state.

**What it tests**:
- Server state is reset to initial game state
- Response contains correct "New game started" message
- Board is reset to empty state

**Why it's important**: Essential for allowing players to start fresh games. This is a fundamental game operation.

### `testGetStateAction`
**Purpose**: Verifies that the "get_state" action returns the current game state without modifying it.

**What it tests**:
- Server state remains unchanged
- Response contains correct "Current game state" message
- Board information is accurately reflected

**Why it's important**: Allows clients to poll the server for current game status without affecting the game state.

### `testMakeMoveActionValid`
**Purpose**: Verifies that valid moves are properly processed and applied to the game state.

**What it tests**:
- Board is updated with the move
- Current player switches (X → O or O → X)
- Response contains appropriate message

**Why it's important**: This is the core functionality for playing the game. Validates the primary game mechanic.

---

## 6. Error Handling Tests

### `testMakeMoveActionInvalid`
**Purpose**: Verifies that invalid moves (out of bounds) are properly rejected without changing the game state.

**What it tests**:
- Server state remains unchanged
- Error message is returned
- Game continues normally

**Why it's important**: Prevents invalid moves from corrupting the game state and provides clear feedback to clients.

### `testMakeMoveActionMissingPosition`
**Purpose**: Verifies that make_move requests without a position are properly handled as errors.

**What it tests**:
- Server state remains unchanged
- Appropriate error message is returned
- Game continues normally

**Why it's important**: Validates proper API usage and prevents malformed requests from causing issues.

### `testUnknownAction`
**Purpose**: Verifies that unknown or unsupported actions are properly handled as errors.

**What it tests**:
- Server state remains unchanged
- Error message is returned
- Game continues normally

**Why it's important**: Provides graceful handling of malformed or unsupported API requests, improving API robustness.

---

## 7. Integration Tests

### `testCompleteGameFlow`
**Purpose**: Simulates a complete game from start to finish using server actions, ending with a winning condition.

**What it tests**:
- Multiple moves can be made sequentially
- Game state progresses correctly
- Winning conditions are properly detected
- Game over state is reached

**Test scenario**: A sequence of moves that results in X winning:
- X plays at (0,0), O plays at (1,1)
- X plays at (0,1), O plays at (2,2)  
- X plays at (0,2) to win

**Why it's important**: Validates the complete game flow through the server API, ensuring that all components work together correctly in a realistic game scenario.

---

## Test Coverage Summary

The server test suite provides comprehensive coverage of:

- **Data integrity**: Board conversions, JSON serialization
- **State management**: Initialization, state transitions
- **API functionality**: All supported actions (new_game, get_state, make_move)
- **Error handling**: Invalid inputs, malformed requests, unknown actions
- **Integration**: Complete game flows with multiple actions

This ensures the server is robust, reliable, and provides a solid foundation for the Tic-Tac-Toe game API.

## Running the Tests

To run all server tests:
```bash
bazel test //haskell/app/tic-tac-toe-server:server_test --test_output=all
```

The tests use a custom test runner that shows individual test labels and results, making it easy to identify which specific functionality is being tested and whether it passes or fails. 