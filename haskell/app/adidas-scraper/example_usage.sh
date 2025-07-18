#!/bin/bash

# Example usage script for Adidas scraper
# This script demonstrates how to use the Adidas scraper

set -e

echo "=== Adidas Scraper Example Usage ==="
echo

# Example 1: Scrape navigation links
echo "1. Scraping navigation links..."
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-nav-links --all --output adidas_nav_links.json
echo "Navigation links saved to adidas_nav_links.json"
echo

# Example 2: Scrape a specific product page (men's shoes)
echo "2. Scraping men's shoes page..."
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url "https://www.adidas.com/us/men-shoes" --output adidas_mens_shoes.json
echo "Men's shoes data saved to adidas_mens_shoes.json"
echo

# Example 3: Scrape women's clothing
echo "3. Scraping women's clothing page..."
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url "https://www.adidas.com/us/women-clothing" --output adidas_womens_clothing.json
echo "Women's clothing data saved to adidas_womens_clothing.json"
echo

# Example 4: Scrape kids' products
echo "4. Scraping kids' products page..."
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url "https://www.adidas.com/us/kids" --output adidas_kids.json
echo "Kids' products data saved to adidas_kids.json"
echo

# Example 5: Scrape sport products
echo "5. Scraping sport products page..."
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url "https://www.adidas.com/us/sport" --output adidas_sport.json
echo "Sport products data saved to adidas_sport.json"
echo

echo "=== All scraping examples completed ==="
echo "Generated files:"
ls -la adidas_*.json
echo
echo "To view the scraped data:"
echo "  cat adidas_nav_links.json"
echo "  cat adidas_mens_shoes.json"
echo "  cat adidas_womens_clothing.json"
echo "  cat adidas_kids.json"
echo "  cat adidas_sport.json" 