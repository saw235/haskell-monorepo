# MTL Compilation Test

This directory contains applications to test MTL (Monad Transformer Library) compilation in a Bazel Haskell project.

## Applications

### 1. Basic MTL Test (`mtl-test`)

**File:** `Main.hs`  
**Target:** `//haskell/app/mtl-test:mtl-test`

A comprehensive test of MTL compilation using a monad stack with:
- `ReaderT Config` - for configuration management
- `StateT AppState` - for state management  
- `ExceptT Text IO` - for error handling and IO

**Monad Stack:** `ReaderT Config (StateT AppState (ExceptT Text IO))`

**Features Tested:**
- ✅ Reader monad operations (`ask`)
- ✅ State monad operations (`get`, `put`, `modify`)
- ✅ Except monad operations (`throwError`, `catchError`)
- ✅ IO operations (`liftIO`)
- ✅ Complex operations combining multiple transformers
- ✅ Error handling and recovery

### 2. Advanced MTL Test (`mtl-test-advanced`)

**File:** `AdvancedMain.hs`  
**Target:** `//haskell/app/mtl-test:mtl-test-advanced`

*Note: This version is currently being debugged due to complex monad stack type issues.*

## Building and Running

### Build both applications:
```bash
bazel build //haskell/app/mtl-test:all
```

### Build individual applications:
```bash
# Basic test
bazel build //haskell/app/mtl-test:mtl-test

# Advanced test (when fixed)
bazel build //haskell/app/mtl-test:mtl-test-advanced
```

### Run the basic test:
```bash
bazel run //haskell/app/mtl-test:mtl-test
```

## Dependencies

The applications use the following packages from the Stackage snapshot:
- `base` - Haskell standard library
- `mtl` - Monad Transformer Library
- `transformers` - Monad transformers
- `text` - Efficient text handling
- `random` - Random number generation

## What This Tests

1. **MTL Compilation**: Verifies that MTL packages compile correctly in the Bazel environment
2. **Monad Transformer Stacking**: Tests complex monad transformer combinations
3. **Type Inference**: Ensures GHC can properly infer types in complex monad stacks
4. **Error Handling**: Demonstrates error handling across transformer boundaries
5. **IO Integration**: Shows how IO operations work within transformer stacks

## Example Output

When running the basic test, you should see output like:

```
=== MTL Compilation Test ===

Reader test: App: MTL Test App, Debug: True, Max Retries: 3
State test: Counter = 1
Except test (success): Success!
Except test (recovery): Recovery successful: This is a test error
Generated random number: 58
IO test: Random number = 58
Generated random number: 35
Complex operation: Input: test-input, Random: 35, Result: Success!, Counter: 1
=== Final State ===
AppState {counter = 1, lastOperation = "complex_operation", userInputs = ["test-input"]}

=== MTL Test Completed Successfully! ===
```

## Troubleshooting

If you encounter compilation errors:

1. **Missing dependencies**: Ensure all required packages are available in your Stackage snapshot
2. **Type inference issues**: Check that your monad stack order is correct
3. **Import errors**: Verify that all MTL modules are properly imported

## Lessons Learned

- The basic monad stack `ReaderT Config (StateT AppState (ExceptT Text IO))` works reliably
- Complex monad stacks with `WriterT` require careful attention to type structure
- The order of transformer composition affects the final result type
- Error handling works seamlessly across transformer boundaries 