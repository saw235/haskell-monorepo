#!/usr/bin/env bash
set -euo pipefail

HTTP_SERVER="$1"
SELENIUM_JAR="$2"
TEST_BIN="$3"
HTML_FILE="$4"
shift 4

SELENIUM_PORT=4444

# Start the HTTP server in the background
"$HTTP_SERVER" "$HTML_FILE" &
HTTP_SERVER_PID=$!

# Start Selenium server in the background
java -jar "$SELENIUM_JAR" -port $SELENIUM_PORT > selenium.log 2>&1 &
SELENIUM_PID=$!

# Wait for HTTP server to be ready
for i in {1..20}; do
  if nc -z localhost 8081; then
    break
  fi
  sleep 0.5
done

# Wait for Selenium to be ready
for i in {1..20}; do
  if nc -z localhost $SELENIUM_PORT; then
    break
  fi
  sleep 0.5
done

# Run the test binary, forwarding any extra args
echo "Running: $TEST_BIN $@"
"$TEST_BIN" "$@"
TEST_EXIT_CODE=$?

# Kill servers
kill $HTTP_SERVER_PID
kill $SELENIUM_PID
wait $HTTP_SERVER_PID 2>/dev/null || true
wait $SELENIUM_PID 2>/dev/null || true

exit $TEST_EXIT_CODE 