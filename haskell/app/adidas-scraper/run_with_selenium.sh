#!/bin/bash

# Script to run adidas-scraper with Selenium server
# Usage: ./run_with_selenium.sh [scraper-args...]
#
# Environment variables:
#   SELENIUM_JAR_PATH: Path to selenium-server-standalone JAR (overrides default)
#   SELENIUM_PORT: Port for Selenium server (default: 4444)

set -e

# Configuration with environment variable support
SELENIUM_PORT="${SELENIUM_PORT:-4444}"
SELENIUM_PID_FILE="/tmp/selenium-server-${SELENIUM_PORT}.pid"

# Find the Selenium JAR file
find_selenium_jar() {
    # If SELENIUM_JAR_PATH is set, use it
    if [ -n "$SELENIUM_JAR_PATH" ]; then
        echo "$SELENIUM_JAR_PATH"
        return 0
    fi
    
    # Use the JAR path provided by Bazel at build time
    JAR_PATH="SELENIUM_JAR_PLACEHOLDER"
    if [ -f "$JAR_PATH" ]; then
        echo "$JAR_PATH"
        return 0
    fi
    
    return 1
}

# Function to cleanup Selenium server
cleanup() {
    if [ -f "$SELENIUM_PID_FILE" ]; then
        PID=$(cat "$SELENIUM_PID_FILE")
        if kill -0 "$PID" 2>/dev/null; then
            echo "Stopping Selenium server (PID: $PID)..."
            kill "$PID"
            wait "$PID" 2>/dev/null || true
        fi
        rm -f "$SELENIUM_PID_FILE"
    fi
}

# Set up cleanup on script exit
trap cleanup EXIT

# Find Selenium JAR
SELENIUM_JAR=$(find_selenium_jar)
if [ $? -ne 0 ]; then
    echo "Error: Selenium JAR not found"
    echo "Please ensure the JAR is available in Bazel runfiles or set SELENIUM_JAR_PATH environment variable"
    echo "Expected location: external/selenium/selenium-server-standalone-3.141.59.jar"
    echo "Custom location: Set SELENIUM_JAR_PATH environment variable"
    exit 1
fi

echo "Using Selenium JAR: $SELENIUM_JAR"

# Check if Selenium server is already running on the specified port
if curl -s "http://localhost:$SELENIUM_PORT/wd/hub/status" > /dev/null 2>&1; then
    echo "Selenium server already running on port $SELENIUM_PORT"
else
    # Check if we have a PID file for this port
    if [ -f "$SELENIUM_PID_FILE" ]; then
        PID=$(cat "$SELENIUM_PID_FILE")
        if kill -0 "$PID" 2>/dev/null; then
            echo "Selenium server already running (PID: $PID) but not responding on port $SELENIUM_PORT"
            echo "Removing stale PID file and starting new server..."
            rm -f "$SELENIUM_PID_FILE"
        else
            echo "Removing stale PID file"
            rm -f "$SELENIUM_PID_FILE"
        fi
    fi

    # Start Selenium server
    echo "Starting Selenium server on port $SELENIUM_PORT..."
    java -jar "$SELENIUM_JAR" -port "$SELENIUM_PORT" > /tmp/selenium-server-${SELENIUM_PORT}.log 2>&1 &
    SELENIUM_PID=$!
    echo "$SELENIUM_PID" > "$SELENIUM_PID_FILE"
    
    # Wait for Selenium server to be ready
    echo "Waiting for Selenium server to be ready..."
    for i in {1..30}; do
        if curl -s "http://localhost:$SELENIUM_PORT/wd/hub/status" > /dev/null 2>&1; then
            echo "Selenium server is ready"
            break
        fi
        if [ $i -eq 30 ]; then
            echo "Error: Selenium server failed to start within 30 seconds"
            echo "Check /tmp/selenium-server-${SELENIUM_PORT}.log for details"
            exit 1
        fi
        sleep 1
    done
fi

# Find the adidas-scraper binary in runfiles
SCRAPER_BIN=""
if [ -n "$RUNFILES_DIR" ]; then
    # Use RUNFILES_DIR if set
    SCRAPER_BIN="$RUNFILES_DIR/_main/haskell/app/adidas-scraper/adidas-scraper"
else
    # Try to find it relative to the script
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    SCRAPER_BIN="$SCRIPT_DIR.runfiles/_main/haskell/app/adidas-scraper/adidas-scraper"
fi

# If still not found, try the direct path from the symlink
if [ ! -f "$SCRAPER_BIN" ]; then
    # Get the directory where this script is located
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    # Look for the binary in the same directory
    SCRAPER_BIN="$SCRIPT_DIR/adidas-scraper"
fi

# Check if the binary exists
if [ ! -f "$SCRAPER_BIN" ]; then
    echo "Error: adidas-scraper binary not found at $SCRAPER_BIN"
    echo "Script directory: $SCRIPT_DIR"
    echo "Available files in script directory:"
    ls -la "$SCRIPT_DIR" 2>/dev/null || echo "Cannot list script directory"
    echo "Available files in runfiles:"
    if [ -n "$RUNFILES_DIR" ]; then
        find "$RUNFILES_DIR" -name "*adidas*" 2>/dev/null || echo "No adidas files found"
    else
        find "$SCRIPT_DIR.runfiles" -name "*adidas*" 2>/dev/null || echo "No adidas files found"
    fi
    exit 1
fi

# Run the adidas-scraper binary with provided arguments
echo "Running adidas-scraper with arguments: $*"
exec "$SCRAPER_BIN" "$@" 