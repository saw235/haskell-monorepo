# Nike Scraper

## Prerequisites

### Chrome Browser
To run the Nike scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

### Selenium Server
To run the nike scrapper, the selenium-server-standalone is required
```
java -jar ./selenium-server-standalone-3.141.59.jar
```

## Known Issues

- **Headless mode does not work reliably:** The scraper may not load all products or scroll correctly in headless mode. It is recommended to use non-headless mode for best results.
- **Manual browser closure:** In non-headless mode, the browser window may not always close automatically after scraping. Please close the browser window manually if it remains open.

## Usage

### Scrape a Product Page

To scrape products from a specific Nike product page and output to a file:

```
bazel run //haskell/app/nike-scraper -- scrape-page --url <PRODUCT_URL> --output products.json
```

To print the scraped products as JSON to stdout:

```
bazel run //haskell/app/nike-scraper -- scrape-page --url <PRODUCT_URL>
```

### Scrape Navigation Links

By default, this will print only `/w/` category links to stdout:

```
bazel run //haskell/app/nike-scraper -- scrape-nav-links
```

To save only `/w/` category links to a file:

```
bazel run //haskell/app/nike-scraper -- scrape-nav-links --output nav_links.json
```

To print **all** navigation links (not just `/w/` links) to stdout:

```
bazel run //haskell/app/nike-scraper -- scrape-nav-links --all
```

To save **all** navigation links to a file:

```
bazel run //haskell/app/nike-scraper -- scrape-nav-links --all --output nav_links.json
```

- If `--output` is omitted, results are printed to stdout.
- If `--all` is omitted, only `/w/` links are included by default.