# Nike Scraper

A Haskell web scraper for Nike products that handles infinite scrolling pages.

## Features

- **Infinite Scrolling Support**: Automatically scrolls through pages that load products dynamically
- **Headless Chrome**: Runs in headless mode for CI/CD compatibility
- **Product Extraction**: Scrapes product name, price, and image URL
- **Safety Limits**: Prevents infinite loops with scroll attempt limits

## How It Works

The scraper uses WebDriver to:
1. Open the Nike products page in a headless Chrome browser
2. Wait for initial page load (3 seconds)
3. Repeatedly scroll to the bottom of the page using JavaScript: `window.scrollTo(0, document.body.scrollHeight)`
4. Wait 2 seconds between scrolls for new content to load
5. Count the number of product cards after each scroll
6. Continue scrolling until no new products are detected
7. Extract the final HTML and scrape all loaded products
8. Limit to maximum 50 scroll attempts to prevent infinite loops

## Prerequisites

### Chrome Browser
To run the Nike scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

## Usage

```bash
bazel run //haskell/app/nike-scraper:nike-scraper
```

## Output

The scraper will output:
- Progress messages showing scroll attempts and product counts
- Final count of scraped products
- Product details (name, price, image URL) for each found product