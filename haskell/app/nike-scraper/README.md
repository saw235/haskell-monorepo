# Nike Scraper

A Haskell-based web scraper that extracts product information from Nike's website with support for infinite scrolling.

## Features

- **Infinite Scrolling Support**: Automatically handles Nike's infinite scrolling product pages by simulating user scrolling behavior
- **Product Extraction**: Scrapes product names, prices, and image URLs
- **Robust Detection**: Uses product count monitoring to reliably detect when all products have been loaded
- **Headless Browser**: Runs in headless Chrome for efficient scraping without GUI overhead

## How Infinite Scrolling Works

The scraper handles infinite scrolling by:

1. Loading the initial Nike products page
2. Counting the current number of products visible
3. Scrolling to the bottom of the page using JavaScript: `window.scrollTo(0, document.body.scrollHeight);`
4. Waiting 5 seconds for new content to load
5. Counting products again to detect if new items were loaded
6. Repeating steps 3-5 until no new products are detected or maximum attempts (10) reached
7. Scraping all loaded products from the final HTML

This approach ensures all products are captured while preventing infinite loops.

### Timing Strategy

The scraper uses conservative timing to handle Nike's loading behavior:
- **Initial load**: 5 seconds to ensure the page is fully loaded
- **Post-scroll wait**: 5 seconds after each scroll to allow new content to load
- **Additional buffer**: 2 seconds extra wait when new products are detected

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
- **Infinite Scroll**: JavaScript execution with product count monitoring
- **Target**: Nike men's shoes category