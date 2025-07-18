#!/bin/bash

# Example usage of the nike-scraper-with-selenium target
# This script demonstrates how to use the new Bazel target that automatically manages the Selenium server

echo "=== Nike Scraper with Selenium Example ==="
echo

# Example 1: Scrape navigation links and save to file
echo "Example 1: Scraping navigation links..."
echo "Command: bazel run //haskell/app/nike-scraper:nike-scraper-with-selenium -- scrape-nav-links --all --output nav_links.json"
echo

# Example 2: Scrape navigation links to stdout
echo "Example 2: Scraping navigation links to stdout..."
echo "Command: bazel run //haskell/app/nike-scraper:nike-scraper-with-selenium -- scrape-nav-links --all"
echo

# Example 3: Scrape a product page
echo "Example 3: Scraping a product page..."
echo "Command: bazel run //haskell/app/nike-scraper:nike-scraper-with-selenium -- scrape-page --url 'https://www.nike.com/w/mens-shoes-5e1x6' --output products.json"
echo

# Example 4: Using custom Selenium configuration
echo "Example 4: Using custom Selenium configuration..."
echo "export SELENIUM_JAR_PATH=/path/to/your/selenium-server-standalone-3.141.59.jar"
echo "export SELENIUM_PORT=4445"
echo "bazel run //haskell/app/nike-scraper:nike-scraper-with-selenium -- scrape-nav-links --all"
echo

echo "=== Key Benefits ==="
echo "1. No need to manually start Selenium server"
echo "2. No need to manually download or place Selenium JAR file"
echo "3. Automatic cleanup of Selenium server on exit"
echo "4. Handles multiple instances gracefully"
echo "5. Configurable via environment variables"
echo "6. Single command execution"
echo "7. Bazel-managed dependencies"
echo

echo "=== Environment Variables ==="
echo "SELENIUM_JAR_PATH: Path to selenium-server-standalone JAR (overrides Bazel-managed JAR)"
echo "SELENIUM_PORT: Port for Selenium server (default: 4444)"
echo

echo "=== Selenium JAR Management ==="
echo "The Selenium JAR file is automatically managed by Bazel and located at:"
echo "external/selenium/selenium-server-standalone-3.141.59.jar"
echo "No manual download or placement required!"
echo

echo "=== Troubleshooting ==="
echo "If you encounter issues:"
echo "1. Check that Java is installed and accessible"
echo "2. Verify the Selenium JAR is available in external/selenium/"
echo "3. Check /tmp/selenium-server-{PORT}.log for server logs"
echo "4. Ensure port 4444 (or your custom port) is available"
echo "5. Use SELENIUM_JAR_PATH to override with a custom JAR if needed" 