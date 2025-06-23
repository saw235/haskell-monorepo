# Nike Scraper

A Haskell-based web scraper for Nike's product pages that handles infinite scrolling to capture all available products.

## Features

- **Infinite Scrolling Support**: Automatically scrolls through Nike's product pages that use infinite loading
- **Chrome WebDriver Integration**: Uses headless Chrome for JavaScript-enabled web scraping
- **Product Data Extraction**: Extracts product names, prices, and image URLs
- **Retry Logic**: Implements smart retry mechanism to ensure all products are loaded

## How Infinite Scrolling Works

The scraper now handles Nike's infinite scrolling by:

1. **Initial Page Load**: Waits for the initial page content to load
2. **Product Counting**: Tracks the number of product cards currently visible
3. **Scroll to Bottom**: Uses JavaScript (`window.scrollTo(0, document.body.scrollHeight)`) to scroll to the bottom
4. **Wait for Loading**: Pauses to allow new content to load dynamically
5. **Retry Logic**: Continues scrolling until no new products are found after 3 consecutive attempts
6. **Final Scraping**: Extracts all product data from the fully loaded page

## Prerequisites

### Chrome Browser
To run the Nike scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```bash
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

## Usage

Build and run the scraper:
```bash
bazel build //haskell/app/nike-scraper:nike-scraper
bazel run //haskell/app/nike-scraper:nike-scraper
```

The scraper will automatically:
- Open Nike's men's shoes page
- Perform infinite scrolling to load all products
- Extract and display product information
- Show progress messages during the scrolling process

## Output

The scraper provides detailed logging:
- Initial product count
- Scrolling progress updates
- Number of new products found after each scroll
- Final count of scraped products
- Individual product details (name, price, image URL)