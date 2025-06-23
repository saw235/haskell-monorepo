# Nike Scraper

A Haskell web scraper for Nike products that handles infinite scrolling pages using Selenium WebDriver.

## Features

- **Infinite Scrolling Support**: Automatically scrolls through pages with infinite scroll to load all available products
- **Robust Scraping**: Uses multiple CSS selectors to handle different page layouts and structures
- **Error Handling**: Comprehensive error handling for network issues and page structure changes
- **Chrome Integration**: Uses headless Chrome browser for reliable scraping

## How It Works

The scraper uses Selenium WebDriver to:

1. Open the Nike products page in a headless Chrome browser
2. Wait for the initial page load
3. Implement infinite scrolling by:
   - Scrolling to the bottom of the page using `window.scrollTo(0, document.body.scrollHeight)`
   - Waiting for new content to load (2 seconds)
   - Checking if the page height has increased
   - Repeating until no new content loads or max scrolls reached (20 scrolls)
4. Extract the final HTML source after all products are loaded
5. Parse products using Scalpel with robust CSS selectors

## Prerequisites

### Chrome Browser
To run the Nike scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```bash
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

### ChromeDriver
ChromeDriver is automatically set up by the project's setup script.

## Building and Running

Build the project using Bazel:
```bash
bazel build //haskell/app/nike-scraper:nike-scraper
```

Run the scraper:
```bash
bazel run //haskell/app/nike-scraper:nike-scraper
```

## Configuration

The scraper is configured to:
- Use headless Chrome with security options (`--no-sandbox`, `--disable-dev-shm-usage`)
- Wait up to 20 scroll iterations to prevent infinite loops
- Wait 2 seconds between scrolls for content to load
- Target the Nike men's shoes page: `https://www.nike.com/w/mens-shoes-nik1zy7ok`

## Output

The scraper outputs:
- Number of products successfully scraped
- For each product:
  - Product name
  - Price
  - Image URL

## Implementation Details

### Infinite Scrolling Algorithm

The infinite scrolling is implemented using the following algorithm:

1. Get initial page height using `document.body.scrollHeight`
2. Scroll to bottom using `window.scrollTo(0, document.body.scrollHeight)`
3. Wait for content to load
4. Get new page height
5. If height hasn't changed, stop (reached end)
6. Otherwise, repeat with new height
7. Maximum 20 iterations to prevent infinite loops

### Robust Scraping

The scraper uses multiple CSS selectors to handle different page structures:

**Product Cards:**
- `div.product-card`
- `div.card`
- `div.product-tile`
- `div.product`

**Product Names:**
- `div.product-card__title`
- `h3.product-card__title`
- `div.product-card__subtitle`
- `[data-testid=product-title]`
- And more...

**Product Prices:**
- `div.product-price`
- `span.product-price`
- `[data-testid=product-price]`
- And more...

This approach ensures the scraper continues to work even if Nike changes their CSS class names or page structure.

## Error Handling

The scraper includes comprehensive error handling for:
- Network connection issues
- Page loading timeouts
- Changes in page structure
- WebDriver session failures

All errors are caught and reported with helpful messages.

## Dependencies

- `webdriver`: For browser automation
- `scalpel`: For HTML parsing
- `text`: For text processing
- `base`: Standard Haskell libraries