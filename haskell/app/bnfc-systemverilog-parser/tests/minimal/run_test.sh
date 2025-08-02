#!/bin/bash

# Script to run the minimal SystemVerilog parser test

set -e

echo "Building and running SystemVerilog minimal parser test..."
echo "========================================================"

# Change to the project root directory
cd "$(dirname "$0")/../.."

# Build the test binary
echo "Building test binary..."
bazel build //haskell/app/bnfc-systemverilog-parser/tests/minimal:test_minimal

# Run the test
echo ""
echo "Running tests..."
bazel run //haskell/app/bnfc-systemverilog-parser/tests/minimal:test_minimal

echo ""
echo "Test completed successfully!" 