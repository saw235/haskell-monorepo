# Tic-Tac-Toe Server Test Suite

This directory contains comprehensive unit tests for the Tic-Tac-Toe server implementation.

## Test Structure

The test suite is organized into three main components:

### 1. GameLogic Tests (`GameLogicTest.hs`)
Tests the core game logic functionality:
- Board initialization and state management
- Move validation and board updates
- Winning condition detection (horizontal, vertical, diagonal)
- Game over conditions (win/draw)
- State monad operations
- Complete game flow scenarios

### 2. Server Tests (`ServerTest.hs`)
Tests the HTTP server functionality:
- Board conversion utilities (JSON serialization)
- Server state management
- API request/response handling
- Game action processing (new game, make move, get state)
- Error handling and validation

### 3. Test Runner (`TestRunner.hs`)
Combines all test suites into a single executable with detailed output.

## Running Tests

### Using Bazel (Recommended)

Run the comprehensive test suite:
```bash
bazel test //haskell/app/tic-tac-toe-server:test_suite
```

Run individual test modules:
```bash
# GameLogic tests only
bazel test //haskell/app/tic-tac-toe-server:game_logic_test

# Server tests only
bazel test //haskell/app/tic-tac-toe-server:server_test
```

### Using GHC directly

If you prefer to run tests directly with GHC:

```bash
# Compile and run GameLogic tests
ghc -o game_logic_test GameLogicTest.hs GameLogic.hs -package HUnit
./game_logic_test

# Compile and run Server tests
ghc -o server_test ServerTest.hs Server.hs GameLogic.hs -package HUnit -package aeson -package bytestring -package stm
./server_test

# Compile and run comprehensive test suite
ghc -o test_runner TestRunner.hs GameLogicTest.hs ServerTest.hs GameLogic.hs Server.hs -package HUnit -package aeson -package bytestring -package stm
./test_runner
```

## Test Coverage

### GameLogic Module Coverage
- ✅ Board initialization and validation
- ✅ Player turn management
- ✅ Move validation (bounds checking, occupied cells)
- ✅ Board state updates
- ✅ Winning condition detection (all 8 possible win patterns)
- ✅ Game over detection (win and draw scenarios)
- ✅ State monad operations
- ✅ Complete game flow testing

### Server Module Coverage
- ✅ JSON serialization/deserialization
- ✅ Board conversion utilities
- ✅ Server state initialization
- ✅ API request processing
- ✅ Response generation
- ✅ Error handling
- ✅ Game action validation
- ✅ Complete server workflow

## Test Categories

### Unit Tests
- Individual function testing
- Edge case validation
- Error condition handling

### Integration Tests
- Complete game flow scenarios
- Server-client interaction simulation
- State persistence across requests

### Property-Based Tests
- Board conversion round-trip validation
- JSON serialization consistency
- Game state invariants

## Adding New Tests

To add new tests:

1. **For GameLogic tests**: Add test cases to `GameLogicTest.hs`
2. **For Server tests**: Add test cases to `ServerTest.hs`
3. **Update the test suite**: Add new test labels to the `tests` list in each file
4. **Run tests**: Verify your new tests pass

### Example Test Structure

```haskell
testNewFeature :: Test
testNewFeature = TestCase $ do
  -- Setup
  let initialState = someInitialState
  
  -- Exercise
  let result = someFunction input
  
  -- Assert
  assertEqual "Expected description" expectedValue result
  assertBool "Boolean condition" someCondition
```

## Test Dependencies

The test suite requires the following Haskell packages:
- `HUnit` - Testing framework
- `aeson` - JSON serialization (for Server tests)
- `bytestring` - Byte string handling
- `stm` - Software Transactional Memory

These are automatically managed by Bazel through the `@stackage` repository.

## Continuous Integration

The test suite is designed to be run in CI/CD pipelines:
- Exit code 0 indicates all tests passed
- Exit code 1 indicates test failures
- Detailed output shows test results and summary

## Debugging Failed Tests

When tests fail:

1. **Check the test output** for specific failure messages
2. **Run individual test modules** to isolate the issue
3. **Add debug logging** to understand the failure
4. **Verify test data** matches expected game state
5. **Check for race conditions** in concurrent tests

## Performance Considerations

- Tests are designed to be fast and deterministic
- No external dependencies or network calls
- Pure functions are tested without IO operations
- STM operations are tested with atomic execution 