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