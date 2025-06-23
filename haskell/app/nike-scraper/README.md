# Nike Scraper

A Haskell web scraper for Nike products that handles infinite scrolling pages.

## Features

- **Infinite Scrolling Support**: Automatically scrolls through Nike product pages that use infinite scrolling to load all available products
- **Headless Browser**: Uses Chrome in headless mode for fast, automated scraping
- **Product Extraction**: Extracts product names, prices, and image URLs
- **Smart Stopping**: Stops scrolling when no new products are loaded after multiple attempts

## How Infinite Scrolling Works

The scraper implements the following strategy to handle infinite scrolling:

1. **Initial Load**: Waits for the initial page to load and counts the initial products
2. **Scroll Loop**: Repeatedly executes JavaScript to scroll to the bottom: `window.scrollTo(0, document.body.scrollHeight);`
3. **Product Detection**: Counts product cards after each scroll to detect new content
4. **Smart Termination**: Stops when no new products are found for 5 consecutive scroll attempts
5. **Content Extraction**: Scrapes all loaded products from the final HTML

## Prerequisites

### Chrome Browser
To run the Nike scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```bash
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

## Usage

The scraper will automatically handle infinite scrolling and provide progress updates:

```
Starting Nike scraper with infinite scrolling...
Fetching URL: https://www.nike.com/w/mens-shoes-nik1zy7ok
Initial products found: 24
Starting infinite scroll to load all products...
Products found: 48
New products loaded, continuing to scroll...
Products found: 72
New products loaded, continuing to scroll...
...
Final products count after scrolling: 120
Successfully scraped 120 products.
```

## Build and Run

```bash
bazel run //haskell/app/nike-scraper:nike-scraper
```