# Adidas Scraper

## Prerequisites

### Chrome Browser
To run the Adidas scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

### Selenium Server
The Selenium server JAR file is automatically managed by Bazel and located in `external/selenium/selenium-server-standalone-3.141.59.jar`. No manual setup required.

## Usage

### Option 1: Automatic Selenium Management (Recommended)

Use the `adidas-scraper-with-selenium` target which automatically starts and manages the Selenium server:

```bash
# Scrape navigation links
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-nav-links --all --output nav_links.json

# Scrape a product page
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url <PRODUCT_URL> --output products.json
```

**Environment Variables:**
- `SELENIUM_JAR_PATH`: Path to selenium-server-standalone JAR (overrides Bazel-managed JAR)
- `SELENIUM_PORT`: Port for Selenium server (default: `4444`)

Example:
```bash
export SELENIUM_JAR_PATH=/path/to/your/selenium-server-standalone-3.141.59.jar
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-nav-links --all
```

### Option 2: Manual Selenium Management

If you prefer to manage the Selenium server manually, first start it in a separate terminal:

```bash
java -jar external/selenium/selenium-server-standalone-3.141.59.jar
```

Then run the scraper:

```bash
bazel run //haskell/app/adidas-scraper -- scrape-nav-links --all --output nav_links.json
```

## Known Issues

- **Headless mode does not work reliably:** The scraper may not load all products or scroll correctly in headless mode. It is recommended to use non-headless mode for best results.
- **Manual browser closure:** In non-headless mode, the browser window may not always close automatically after scraping. Please close the browser window manually if it remains open.

## Usage Examples

### Scrape a Product Page

To scrape products from a specific Adidas product page and output to a file:

```
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url <PRODUCT_URL> --output products.json
```

To print the scraped products as JSON to stdout:

```
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-page --url <PRODUCT_URL>
```

### Scrape Navigation Links

By default, this will print only `/us/` category links to stdout:

```
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-nav-links
```

To save only `/us/` category links to a file:

```
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-nav-links --output nav_links.json
```

To print **all** navigation links (not just `/us/` links) to stdout:

```
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-nav-links --all
```

To save **all** navigation links to a file:

```
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- scrape-nav-links --all --output nav_links.json
```

- If `--output` is omitted, results are printed to stdout.
- If `--all` is omitted, only `/us/` links are included by default.

## Common Adidas URLs

The scraper includes some common Adidas category URLs by default:
- https://www.adidas.com/us/men
- https://www.adidas.com/us/women
- https://www.adidas.com/us/kids
- https://www.adidas.com/us/sport 