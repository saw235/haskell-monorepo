# Nike Scraper

A Haskell-based web scraper that extracts product information from Nike's website, with support for infinite scrolling to capture all available products.

## Features

- **Infinite Scrolling**: Automatically scrolls through the entire page to load all products
- **Headless Chrome**: Runs in headless mode for efficient scraping
- **Product Extraction**: Extracts product names, prices, and images
- **Robust Error Handling**: Includes timeouts and retry logic

## Prerequisites

### Chrome Browser
To run the Nike scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```bash
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

## How It Works

The scraper uses the following process:

1. **Page Loading**: Opens the Nike men's shoes page using Chrome WebDriver
2. **Infinite Scrolling**: Repeatedly scrolls to the bottom of the page using JavaScript execution
   - Executes `window.scrollTo(0, document.body.scrollHeight)` to scroll to bottom
   - Waits 2 seconds for new content to load after each scroll
   - Monitors page height changes to detect when new products are loaded
   - Continues until no new content is loaded or maximum attempts (50) are reached
3. **Content Extraction**: After all products are loaded, extracts the full HTML source
4. **Product Parsing**: Uses Scalpel library to parse product information from HTML

## Usage

Build and run the scraper:
```bash
bazel build //haskell/app/nike-scraper:nike-scraper
bazel run //haskell/app/nike-scraper:nike-scraper
```

The scraper will output the number of products found and display each product's details.

## Technical Details

- **Language**: Haskell
- **WebDriver**: Uses `webdriver` library for browser automation
- **HTML Parsing**: Uses `scalpel` library for HTML scraping
- **JavaScript Execution**: Leverages WebDriver's `executeJS` for infinite scrolling
- **Concurrent Operations**: Includes proper delays and timing for dynamic content loading