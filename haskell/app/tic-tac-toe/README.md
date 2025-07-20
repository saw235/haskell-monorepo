# Tic-Tac-Toe - Haskell Backend

This directory contains the Haskell backend for the Tic-Tac-Toe game, including both the original CLI version and the HTTP server for the Electron frontend.

## Project Structure

The Tic-Tac-Toe project is now organized into two separate directories:

- **`tic-tac-toe/`** (this directory) - Haskell backend
- **`tic-tac-toe-electron/`** - Electron frontend

## Files

- `Main.hs` - Original CLI game logic and pure functional game engine
- `Server.hs` - HTTP API server implementation for Electron frontend
- `ServerMain.hs` - Server entry point
- `BUILD.bazel` - Bazel build configuration
- `README.md` - This file

## Building and Running

### CLI Version (Original)
```bash
# Build the CLI version
bazel build //haskell/app/tic-tac-toe:tic-tac-toe

# Run the CLI version
bazel run //haskell/app/tic-tac-toe:tic-tac-toe
```

### Server Version (for Electron Frontend)
```bash
# Build the server
bazel build //haskell/app/tic-tac-toe:tic-tac-toe-server

# Run the server
bazel run //haskell/app/tic-tac-toe:tic-tac-toe-server
```

The server will start on `http://localhost:3000` and provide HTTP API endpoints for the Electron frontend.

## API Endpoints

- `GET /` - Get current game state
- `POST /` - Make game actions
  - `{"action": "new_game"}` - Start a new game
  - `{"action": "make_move", "position": [row, col]}` - Make a move
  - `{"action": "get_state"}` - Get current state

## Game Logic

The game logic is implemented in pure Haskell using the State monad, ensuring:

- **Immutability**: Game state is never modified in place
- **Purity**: All game logic is pure functions
- **Type Safety**: Strong typing prevents invalid game states
- **Testability**: Pure functions are easy to test

## Architecture

- **Game Engine**: Pure functional game logic in `Main.hs`
- **HTTP Server**: WAI-based server in `Server.hs`
- **State Management**: Uses State monad for game state
- **JSON API**: Aeson for JSON serialization/deserialization

## Frontend

For the Electron frontend, see the `../tic-tac-toe-electron/` directory.

## Development

To modify the game logic, edit `Main.hs`. To modify the server behavior, edit `Server.hs`. The server automatically re-exports all game logic from `Main.hs`. 