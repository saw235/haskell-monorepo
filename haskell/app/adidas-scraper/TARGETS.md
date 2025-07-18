# Adidas Scraper - Available Bazel Targets

This document lists all available Bazel targets for the Adidas scraper project.

## Main Scraper Targets

### 1. `adidas-scraper`
The main scraper binary without Selenium management.
```bash
bazel build //haskell/app/adidas-scraper:adidas-scraper
```

### 2. `adidas-scraper-with-selenium` ⭐ **Recommended**
The main scraper with automatic Selenium server management.
```bash
# Scrape products from a specific URL
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page --url "https://www.adidas.com/us/men-shoes" \
  --output products.json --delay 3

# Scrape navigation links
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-nav-links --all --output nav_links.json

# Use different fingerprint profiles
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page --url "https://www.adidas.com/us/men-shoes" \
  --output products.json --delay 3 --profile firefox
```

## Fingerprint Testing Targets

### 3. `fingerprint-test`
Interactive fingerprint test binary without Selenium management.
```bash
bazel build //haskell/app/adidas-scraper:fingerprint-test
```

### 4. `fingerprint-test-with-selenium` ⭐ **Interactive Testing**
Interactive fingerprint test with Selenium management. Prompts for test choice.
```bash
bazel run //haskell/app/adidas-scraper:fingerprint-test-with-selenium
```
**Options:**
- `1` - Basic fingerprint detection test
- `2` - Profile-based fingerprint test

### 5. `fingerprint-test-simple`
Non-interactive fingerprint test binary without Selenium management.
```bash
bazel build //haskell/app/adidas-scraper:fingerprint-test-simple
```

### 6. `fingerprint-test-simple-with-selenium` ⭐ **Automated Testing**
Non-interactive fingerprint test with Selenium management. Runs all tests automatically.
```bash
bazel run //haskell/app/adidas-scraper:fingerprint-test-simple-with-selenium
```

### 7. `fingerprint-test-auto-with-selenium` ⭐ **Quick Auto Test**
Completely non-interactive fingerprint test that runs quickly and automatically.
```bash
bazel run //haskell/app/adidas-scraper:fingerprint-test-auto-with-selenium
```

## Internal Targets

### 7. `adidas-scraper-wrapper`
Generated wrapper script for the main scraper.
```bash
bazel build //haskell/app/adidas-scraper:adidas-scraper-wrapper
```

### 8. `fingerprint-test-wrapper`
Generated wrapper script for the interactive fingerprint test.
```bash
bazel build //haskell/app/adidas-scraper:fingerprint-test-wrapper
```

### 9. `fingerprint-test-simple-wrapper`
Generated wrapper script for the simple fingerprint test.
```bash
bazel build //haskell/app/adidas-scraper:fingerprint-test-simple-wrapper
```

## Usage Examples

### Scraping Products
```bash
# Basic scraping with default profile
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page \
  --url "https://www.adidas.com/us/men-shoes" \
  --output men_shoes.json \
  --delay 3

# Scraping with Firefox profile
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page \
  --url "https://www.adidas.com/us/women-shoes" \
  --output women_shoes.json \
  --delay 3 \
  --profile firefox

# Scraping with Chrome profile
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-page \
  --url "https://www.adidas.com/us/kids-shoes" \
  --output kids_shoes.json \
  --delay 3 \
  --profile chrome
```

### Scraping Navigation Links
```bash
# Scrape all navigation links
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-nav-links \
  --all \
  --output all_nav_links.json

# Scrape only category links (default)
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- \
  scrape-nav-links \
  --output category_links.json
```

### Testing Anti-Fingerprinting
```bash
# Interactive fingerprint test
bazel run //haskell/app/adidas-scraper:fingerprint-test-with-selenium

# Automated fingerprint test (runs all tests)
bazel run //haskell/app/adidas-scraper:fingerprint-test-simple-with-selenium

# Quick auto test (fastest, no delays)
bazel run //haskell/app/adidas-scraper:fingerprint-test-auto-with-selenium
```

## Fingerprint Profiles

The scraper supports different fingerprint profiles to avoid detection:

- **`default`** - Firefox on Linux (default)
- **`firefox`** - Firefox-specific profile
- **`chrome`** - Chrome-specific profile  
- **`safari`** - Safari-specific profile

## Build All Targets
```bash
# Build everything
bazel build //haskell/app/adidas-scraper/...

# List all targets
bazel query //haskell/app/adidas-scraper/...
```

## Testing Fingerprint Detection

The fingerprint tests open a browser to https://bot.sannysoft.com, which provides a comprehensive fingerprint detection test. Check the browser for:

- ✅ Green indicators = Good (not detected)
- ❌ Red indicators = Detected (needs improvement)
- 🟡 Yellow indicators = Partial detection

## Troubleshooting

### Selenium Issues
If you encounter Selenium connection issues:
```bash
# Kill existing Selenium processes
pkill -f selenium-server

# Restart the scraper
bazel run //haskell/app/adidas-scraper:adidas-scraper-with-selenium -- ...
```

### Build Issues
If you encounter build issues:
```bash
# Clean and rebuild
bazel clean --expunge
bazel build //haskell/app/adidas-scraper/...
```

## Notes

- All targets with `-with-selenium` automatically manage the Selenium server
- The scraper runs in non-headless mode by default for better anti-detection
- Fingerprint tests are designed for manual inspection of browser behavior
- Use different profiles if one gets detected by Adidas 