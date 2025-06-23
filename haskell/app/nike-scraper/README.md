# Nike Scraper

A Haskell-based web scraper that extracts product information from Nike's website with support for infinite scrolling.

## Features

- **Infinite Scrolling Support**: Automatically handles Nike's infinite scrolling product pages by simulating user scrolling behavior
- **Product Extraction**: Scrapes product names, prices, and image URLs
- **Robust Detection**: Uses product count monitoring to reliably detect when all products have been loaded
- **Headless Browser**: Runs in headless Chrome for efficient scraping without GUI overhead

## How Infinite Scrolling Works

The scraper intelligently handles infinite scrolling by:

1. Loading the initial Nike products page
2. **Extracting the total expected product count** from the header element `<span class="wall-header__item_count">(639)</span>`
3. Counting the current number of products visible
4. Scrolling to the bottom of the page using JavaScript: `window.scrollTo(0, document.body.scrollHeight);`
5. Waiting 7 seconds for new content to load
6. Comparing current product count against the expected total
7. Repeating steps 4-6 until all expected products are loaded or maximum attempts (10) reached
8. Scraping all loaded products from the final HTML

### Smart Detection Strategy

- **Primary Method**: Uses the total count from Nike's header to know exactly how many products to expect
- **Multiple Selectors**: Tries span, h1, and header selectors to find the product count
- **Progress Tracking**: Shows real-time progress like "Progress: 120/639 products"
- **Dynamic Content Handling**: Waits for JavaScript-loaded content and retries total count extraction
- **Fallback Method**: If total count can't be determined, falls back to detecting when no new products load
- **Safety Limits**: Maximum attempts prevent infinite loops

This approach ensures all products are captured efficiently and provides clear progress feedback.

### Timing Strategy

The scraper uses conservative timing to handle Nike's loading behavior:
- **Initial load**: 8 seconds to ensure the page is fully loaded (increased for dynamic content)
- **Post-scroll wait**: 7 seconds after each scroll to allow new content to load
- **Additional buffer**: 4 seconds extra wait when new products are detected
- **Retry logic**: Only gives up after 2 consecutive failed scroll attempts

## Prerequisites

### Chrome Browser
To run the Nike scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```bash
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

## Usage

```bash
bazel run //haskell/app/nike-scraper
```

## Technical Details

- **Language**: Haskell
- **Web Driver**: Chrome WebDriver via `webdriver` library
- **HTML Parsing**: Scalpel library for CSS selector-based scraping
- **Infinite Scroll**: JavaScript execution with intelligent total count extraction and progress monitoring
- **Target**: Nike men's shoes category