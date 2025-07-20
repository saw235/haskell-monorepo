#!/bin/bash

# Flag to prevent multiple cleanup calls
CLEANUP_DONE=false

# Function to cleanup on exit
cleanup() {
    if [ "$CLEANUP_DONE" = true ]; then
        return
    fi
    CLEANUP_DONE=true
    
    echo "Cleaning up..."
    if [ ! -z "$SERVER_PID" ]; then
        echo "Stopping Haskell server (PID: $SERVER_PID)..."
        kill $SERVER_PID 2>/dev/null || true
        wait $SERVER_PID 2>/dev/null || true
    fi
    if [ ! -z "$ELECTRON_PID" ]; then
        echo "Stopping Electron app (PID: $ELECTRON_PID)..."
        kill $ELECTRON_PID 2>/dev/null || true
        wait $ELECTRON_PID 2>/dev/null || true
    fi
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM EXIT

# Get the directory where the runfiles are located
SCRIPT_DIR="$(dirname "$0")"
RUNFILES_DIR="${SCRIPT_DIR}/tic-tac-toe.runfiles"

# Find the server binary using $location if available, otherwise fallback
if [ ! -z "$1" ] && [ "$1" != "--dev" ]; then
    # If first argument is a path, use it as server location
    SERVER_BINARY="$1"
else
    # Default fallback path
    SERVER_BINARY="${RUNFILES_DIR}/_main/haskell/app/tic-tac-toe-server/server"
fi

if [ ! -f "$SERVER_BINARY" ]; then
    echo "Error: Server binary not found at $SERVER_BINARY"
    echo "Available files in runfiles:"
    find "${RUNFILES_DIR}" -name "*server*" -type f 2>/dev/null || echo "No server files found"
    exit 1
fi

# Start the Haskell server
echo "Starting Haskell server..."
cd "$(dirname "$SERVER_BINARY")"
"$SERVER_BINARY" &
SERVER_PID=$!
echo "Haskell server started with PID: $SERVER_PID"

# Wait a moment for server to start
sleep 2

# Check if server is running
if ! kill -0 $SERVER_PID 2>/dev/null; then
    echo "Error: Haskell server failed to start"
    exit 1
fi

# Start the Electron app
echo "Starting Electron app..."
cd "${RUNFILES_DIR}/_main/electron-app/tic-tac-toe"
pnpm dlx electron . "$@" &
ELECTRON_PID=$!
echo "Electron app started with PID: $ELECTRON_PID"

# Wait for Electron process to exit and then cleanup
wait $ELECTRON_PID
echo "Electron app closed, cleaning up..."
cleanup 