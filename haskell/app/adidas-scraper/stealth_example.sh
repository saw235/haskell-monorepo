#!/bin/bash

# Stealth scraping example for Adidas with anti-fingerprinting
# This script demonstrates advanced techniques to avoid detection

set -e

echo "=== Adidas Stealth Scraper Example ==="
echo "Using advanced anti-fingerprinting techniques"
echo

# Set environment variables for stealth mode
export ADIDAS_USE_STEALTH=true
export ADIDAS_DELAY=5000
export ADIDAS_MAX_RETRIES=5
export ADIDAS_TIMEOUT=60000

echo "Configuration:"
echo "  Stealth mode: $ADIDAS_USE_STEALTH"
echo "  Delay between requests: ${ADIDAS_DELAY}ms"
echo "  Max retries: $ADIDAS_MAX_RETRIES"
echo "  Session timeout: ${ADIDAS_TIMEOUT}ms"
echo

# Example 1: Stealth navigation scraping
echo "1. Stealth navigation scraping..."
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-nav-links --all --output stealth_nav_links.json
echo "Stealth navigation links saved to stealth_nav_links.json"
echo

# Example 2: Stealth product scraping with longer delays
echo "2. Stealth product scraping (men's shoes)..."
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url "https://www.adidas.com/us/men-shoes" --output stealth_mens_shoes.json --delay 5
echo "Stealth men's shoes data saved to stealth_mens_shoes.json"
echo

# Example 3: Stealth product scraping with different category
echo "3. Stealth product scraping (women's clothing)..."
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url "https://www.adidas.com/us/women-clothing" --output stealth_womens_clothing.json --delay 5
echo "Stealth women's clothing data saved to stealth_womens_clothing.json"
echo

echo "=== Stealth scraping completed ==="
echo "Generated files:"
ls -la stealth_*.json
echo
echo "Stealth techniques used:"
echo "  ✓ Anti-fingerprinting JavaScript patches"
echo "  ✓ User-Agent rotation"
echo "  ✓ WebGL fingerprint randomization"
echo "  ✓ Canvas fingerprint randomization"
echo "  ✓ Audio fingerprint randomization"
echo "  ✓ Humanized scrolling behavior"
echo "  ✓ Random delays between actions"
echo "  ✓ Chrome automation detection bypass"
echo "  ✓ Navigator properties spoofing"
echo
echo "To view the scraped data:"
echo "  cat stealth_nav_links.json"
echo "  cat stealth_mens_shoes.json"
echo "  cat stealth_womens_clothing.json" 