# WebSocket Client - Electron App

This Electron application demonstrates real-time communication between an Electron frontend and a Haskell WebSocket server backend, including keyboard input streaming.

## Features

- **Real-time WebSocket communication** with Haskell backend
- **Ping/Pong functionality** for connectivity testing
- **Echo messages** with response confirmation
- **Calculator** that processes mathematical expressions on the Haskell server
- **Real-time keyboard input** - Stream arrow key presses to backend
- **Custom message sending** with multiple message types
- **Live connection status** indicator
- **Message history** with timestamps and color-coded message types

## Quick Start

### Run Everything at Once

The easiest way to test the complete system:

```bash
bazel run //electron-app/websocket-client:websocket-demo
```

This starts both the Haskell WebSocket server and the Electron client automatically.

For development mode with DevTools:

```bash
bazel run //electron-app/websocket-client:websocket-demo-dev
```

### Manual Setup (Alternative)

#### 1. Start the Haskell WebSocket Server

```bash
bazel run //haskell/app/websocket-server:websocket-server
```

The server will start on `localhost:9160`.

#### 2. Install Dependencies and Run the Electron App

```bash
bazel run -- @pnpm//:pnpm --dir $PWD/electron-app/websocket-client/ install
cd electron-app/websocket-client && npm start
```

## Using the Application

### Basic Communication

1. **Connect**: Click "Connect" to establish WebSocket connection
2. **Ping/Pong**: Use "Send Ping" to test connectivity
3. **Echo**: Use "Send Echo" to test message echoing
4. **Calculator**: Enter math expressions like "5 + 3" or "10*2"
5. **Help**: Send "help" message to see all available commands

### Real-time Keyboard Input

1. **Enable**: Check the "Enable Keyboard Input" checkbox
2. **Focus**: Click in the keyboard input area
3. **Use Arrow Keys**: Press ↑ ↓ ← → to send real-time input
4. **Track Position**: Watch the coordinate display update in real-time

The keyboard feature demonstrates streaming input where each arrow key press:
- Updates a virtual position coordinate system
- Immediately sends the key data to the Haskell backend
- Receives acknowledgment with position information

## Supported Message Types

| Message Type | Purpose | Example Content |
|--------------|---------|-----------------|
| `ping` | Test connectivity | any content |
| `echo` | Echo messages back | "Hello World" |
| `calculate` | Math operations | "10 + 5" or "10*5" |
| `keyboard` | Arrow key input | Auto-generated JSON |
| `help` | Show available commands | any content |

## Supported Calculator Operations

- **Addition**: `5 + 3` or `5+3`
- **Subtraction**: `10 - 2` or `10-2`
- **Multiplication**: `4 * 7` or `4*7`
- **Division**: `15 / 3` or `15/3`

Both spaced and non-spaced formats are supported.

## Architecture

### Frontend (Electron)
- **Technology**: Electron app with vanilla JavaScript
- **UI**: Modern glassmorphism design with real-time updates
- **Input Handling**: Mouse clicks, form inputs, and keyboard events
- **Communication**: WebSocket client sending JSON messages

### Backend (Haskell)
- **Technology**: Haskell WebSocket server using `websockets` library
- **Message Processing**: JSON parsing and structured responses
- **Capabilities**: Math computation, echo services, keyboard event handling
- **Real-time**: Immediate response to all message types

### Communication Protocol
- **Transport**: WebSocket over TCP (ws://127.0.0.1:9160)
- **Format**: JSON messages with `msgType` and `content` fields
- **Message Flow**: Request-response pattern with immediate acknowledgment

### Message Format Examples

**Client to Server:**
```json
{"msgType": "calculate", "content": "10 + 5"}
{"msgType": "keyboard", "content": "{\"key\":\"up\",\"direction\":\"north\",\"position\":{\"x\":0,\"y\":1}}"}
```

**Server to Client:**
```json
{"type": "calculation_result", "content": "15.0"}
{"type": "keyboard_ack", "content": "Key: up (north) at position (0,1)"}
```

## Development

### Project Structure
```
electron-app/websocket-client/
├── BUILD.bazel           # Bazel build configuration
├── package.json          # NPM dependencies
├── main.js              # Electron main process
├── renderer.js          # UI logic and WebSocket handling
├── preload.js           # Electron security bridge
├── index.html           # Application UI
├── styles.css           # Modern styling
└── README.md            # This file
```

### Bazel Targets
- `//electron-app/websocket-client:websocket-demo` - Run complete system
- `//electron-app/websocket-client:websocket-demo-dev` - Development mode
- `//electron-app/websocket-client:electron-websocket-client` - Client only
- `//haskell/app/websocket-server:websocket-server` - Server only

This project demonstrates advanced real-time communication patterns between functional programming (Haskell) backends and modern desktop applications (Electron).