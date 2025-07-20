# Tic-Tac-Toe Electron App

A modern Tic-Tac-Toe game built with Electron and powered by a Haskell backend server, demonstrating clean separation between frontend and backend with proper process management.

## Features

- 🎮 Classic Tic-Tac-Toe gameplay
- 🎨 Modern, responsive UI with animations
- 🔄 Real-time game state synchronization
- 🌐 IPC communication with Haskell backend
- ⌨️ Keyboard shortcuts (N for new game, R for reset)
- 📱 Responsive design for different screen sizes
- 🔌 Connection status indicator
- 🏆 Winner announcement with animations
- 🔧 Automatic process management (server + app lifecycle)
- 🚀 Bazel integration for reproducible builds

## Architecture

This app demonstrates a clean separation between frontend and backend with proper process management:

```
┌─────────────────────────────────────────────────────────────┐
│                    Bazel Wrapper Script                     │
│              (run-tic-tac-toe.sh)                          │
│  ┌─────────────────┐    HTTP API    ┌─────────────────┐    │
│  │   Electron App  │ ◄────────────► │ Haskell Server  │    │
│  │   (Frontend)    │                │   (Backend)     │    │
│  └─────────────────┘                └─────────────────┘    │
│           │                                   │             │
│           │ IPC                               │             │
│           ▼                                   ▼             │
│  ┌─────────────────┐                ┌─────────────────┐    │
│  │   Renderer      │                │   Game Logic    │    │
│  │   (UI/UX)       │                │   (Pure)        │    │
│  └─────────────────┘                └─────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Component Details

#### 1. **Bazel Wrapper Script** (`run-tic-tac-toe.sh`)
- **Purpose**: Orchestrates the startup and shutdown of both server and app
- **Features**:
  - Starts Haskell server first and waits for it to be ready
  - Launches Electron app once server is confirmed running
  - Handles graceful shutdown of both processes
  - Prevents zombie processes with proper signal handling
  - Uses `$location` for reliable path resolution

#### 2. **Electron App** (Frontend)
- **Main Process** (`main.js`): Handles IPC and HTTP communication
- **Renderer Process** (`renderer.js`): Game UI and user interactions
- **Preload Script** (`preload.js`): Secure IPC bridge
- **Features**:
  - Always quits when window is closed (ensures cleanup)
  - HTTP client for server communication
  - IPC handlers for game actions

#### 3. **Haskell Server** (Backend)
- **Server** (`Server.hs`): HTTP API endpoints
- **Game Logic** (`GameLogic.hs`): Pure game state management
- **Features**:
  - RESTful API for game operations
  - STM-based state management
  - JSON serialization/deserialization
  - Debug logging for development

### Communication Flow

1. **User Action** → Renderer Process
2. **Renderer** → Main Process (via IPC)
3. **Main Process** → Haskell Server (via HTTP)
4. **Server** → Game Logic (pure functions)
5. **Response** flows back through the same path

## Prerequisites

- **Node.js** and **npm** (for Electron dependencies)
- **Haskell GHC** and **Cabal/Stack** (for backend)
- **Bazel** (for build system and process management)
- **pnpm** (for Electron package management)

## Quick Start

### Option 1: Bazel Wrapper (Recommended)

Start both server and app with a single command:

```bash
# From the repository root
bazel run //electron-app/tic-tac-toe:tic-tac-toe

# For development with DevTools
bazel run //electron-app/tic-tac-toe:tic-tac-toe-dev
```

This will:
1. Start the Haskell server on port 8081
2. Wait for server to be ready
3. Launch the Electron app
4. Handle cleanup when you close the app

### Option 2: Manual Start

If you prefer to start components separately:

```bash
# Terminal 1: Start Haskell server
bazel run //haskell/app/tic-tac-toe-server:server

# Terminal 2: Start Electron app
bazel run //electron-app/tic-tac-toe:electron-tic-tac-toe
```

## Project Structure

```
electron-app/tic-tac-toe/
├── main.js                    # Electron main process (IPC + HTTP client)
├── preload.js                 # Preload script (secure IPC bridge)
├── renderer.js                # Frontend game logic and UI
├── index.html                 # Game UI structure
├── styles.css                 # Modern styling and animations
├── package.json               # Node.js dependencies and scripts
├── BUILD.bazel               # Bazel build configuration
├── run-tic-tac-toe.sh        # Wrapper script for process management
├── README.md                 # This file
└── node_modules/             # Node.js dependencies

haskell/app/tic-tac-toe-server/
├── ServerMain.hs             # Server entry point
├── Server.hs                 # HTTP API implementation
├── GameLogic.hs              # Pure game logic
└── BUILD.bazel              # Bazel build configuration
```

## API Reference

### Haskell Server Endpoints

#### `GET /`
Get current game state.

**Response:**
```json
{
  "board": [["X"," ","O"],[" ","X"," "],[" "," "," "]],
  "currentPlayer": "O",
  "gameOver": false,
  "winner": null,
  "message": "Game in progress",
  "validMoves": [[0,1],[1,0],[1,2],[2,0],[2,1],[2,2]]
}
```

#### `POST /`
Make game actions.

**Request Body:**
```json
{
  "action": "new_game"
}
```
or
```json
{
  "action": "make_move",
  "position": [1, 1]
}
```

### IPC Communication

The Electron app uses IPC for secure communication between main and renderer processes:

- `game-new-game`: Start a new game
- `game-make-move`: Make a move at a specific position
- `game-get-state`: Get the current game state

## Development

### Building

```bash
# Build the complete application
bazel build //electron-app/tic-tac-toe:tic-tac-toe

# Build individual components
bazel build //haskell/app/tic-tac-toe-server:server
bazel build //electron-app/tic-tac-toe:package
```

### Development Mode

```bash
# Run with DevTools enabled
bazel run //electron-app/tic-tac-toe:tic-tac-toe-dev
```

### Adding Features

1. **Game Logic**: Modify `haskell/app/tic-tac-toe-server/GameLogic.hs`
2. **API**: Update `haskell/app/tic-tac-toe-server/Server.hs`
3. **UI**: Enhance `electron-app/tic-tac-toe/renderer.js`
4. **Styling**: Update `electron-app/tic-tac-toe/styles.css`
5. **Process Management**: Modify `electron-app/tic-tac-toe/run-tic-tac-toe.sh`

## Troubleshooting

### Process Management Issues

**Server doesn't start:**
```bash
# Check if port 8081 is available
netstat -tulpn | grep 8081

# Verify server binary exists
bazel build //haskell/app/tic-tac-toe-server:server
```

**App doesn't connect to server:**
- Check connection status indicator in the app
- Verify server is running: `curl http://localhost:8081`
- Look for errors in browser console (F12)

### Build Issues

```bash
# Clean and rebuild
bazel clean
bazel build //electron-app/tic-tac-toe:tic-tac-toe

# Check dependencies
bazel query --output=location //electron-app/tic-tac-toe:tic-tac-toe
```

### Permission Issues

```bash
# Make wrapper script executable
chmod +x electron-app/tic-tac-toe/run-tic-tac-toe.sh
```

## How to Play

1. **Start the game** using one of the methods above
2. **Click on any empty cell** to make a move
3. **Players alternate turns** (X and O)
4. **Get three in a row** (horizontally, vertically, or diagonally) to win
5. **Use "New Game" button** or press `N` to start a fresh game
6. **Use "Reset" button** or press `R` to reset the current game

## Keyboard Shortcuts

- `N` - Start a new game
- `R` - Reset the current game

## Future Enhancements

- 🤖 AI opponent with different difficulty levels
- 🌐 Multiplayer support over WebSocket
- 📊 Game history and statistics
- 🔊 Sound effects and music
- 🎨 Custom themes and skins
- 🏆 Tournament mode
- 📱 Mobile app version
- 🔄 Game replay functionality

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly with both development and production builds
5. Submit a pull request

## License

MIT License - see the main repository license for details. 