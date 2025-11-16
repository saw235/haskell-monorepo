#!/bin/bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Set the Selenium JAR path
SELENIUM_JAR="SELENIUM_JAR_PLACEHOLDER"

# Check if Selenium server is already running
if ! pgrep -f "selenium-server" > /dev/null; then
    echo "Starting Selenium server..."
    java -jar "$SELENIUM_JAR" &
    SELENIUM_PID=$!
    
    # Wait for Selenium to start
    sleep 3
    
    # Check if Selenium started successfully
    if ! pgrep -f "selenium-server" > /dev/null; then
        echo "Failed to start Selenium server"
        exit 1
    fi
    
    echo "Selenium server started with PID: $SELENIUM_PID"
else
    echo "Selenium server already running on port 4444"
fi

# Run the auto fingerprint test
echo "Running auto fingerprint test..."
"$SCRIPT_DIR/fingerprint-test-auto" "$@"

# Clean up Selenium server if we started it
if [ ! -z "$SELENIUM_PID" ]; then
    echo "Stopping Selenium server..."
    kill $SELENIUM_PID
    wait $SELENIUM_PID 2>/dev/null
fi 