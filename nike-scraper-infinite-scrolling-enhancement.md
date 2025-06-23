# Nike Scraper Infinite Scrolling Enhancement

## Issue Summary
**GitHub Issue**: [#3 - Enhance nike-scraper to handle infinite scrolling](https://github.com/saw235/haskell-monorepo/issues/3)

**Problem**: The Nike products page uses infinite scrolling where new rows of products are loaded as the user scrolls down. The original scraper only captured the initially loaded products, missing products that would be loaded through scrolling.

## Solution Implemented

### Key Changes

#### 1. Enhanced Main.hs (`haskell/app/nike-scraper/Main.hs`)

**Added infinite scrolling functionality:**
- **`scrollToBottom`**: Function to execute JavaScript `window.scrollTo(0, document.body.scrollHeight);` and wait for content to load
- **`countCurrentProducts`**: Function to count products on the current page to detect if new products have been loaded
- **`scrollUntilComplete`**: Recursive function that continues scrolling until no new products are detected

**Enhanced workflow:**
1. Load the Nike products page
2. Count initial products
3. Scroll to bottom and wait for content to load
4. Count products again
5. If new products were loaded, continue scrolling
6. Once no new products are detected, proceed with scraping
7. Scrape all products from the fully loaded page

#### 2. Enhanced Scraper.hs (`haskell/app/nike-scraper/src/Nike/Scraper.hs`)

**Added `countProducts` function:**
- Counts the number of product card elements on the page
- Used to determine when infinite scrolling is complete
- Returns an integer count of `div` elements with `product-card` class

### Technical Implementation Details

**JavaScript Execution:**
- Uses WebDriver's `executeJS` function to run `window.scrollTo(0, document.body.scrollHeight);`
- Ensures the page scrolls to the absolute bottom to trigger loading of new products

**Wait Strategy:**
- 2-second delay after each scroll to allow content to load
- Uses `threadDelay 2000000` (2 seconds in microseconds)

**Detection Logic:**
- Compares product count before and after scrolling
- Continues scrolling if `newCount > previousCount`
- Stops when no new products are detected

**Error Handling:**
- Uses `liftIO` for proper IO operations within the WebDriver monad
- Maintains existing error handling for scraping operations

### Code Structure

```haskell
-- Main scrolling logic
scrollUntilComplete :: Int -> WD ()
scrollUntilComplete previousCount = do
    liftIO $ putStrLn $ "Current product count: " ++ show previousCount
    scrollToBottom
    newCount <- countCurrentProducts
    liftIO $ putStrLn $ "After scroll product count: " ++ show newCount
    
    if newCount > previousCount
        then do
            liftIO $ putStrLn "New products loaded, continuing to scroll..."
            scrollUntilComplete newCount
        else do
            liftIO $ putStrLn "No new products loaded, scrolling complete."

-- Product counting for infinite scroll detection
countProducts :: Scraper String Int
countProducts = do
    products <- attrs "class" ("div" @: [hasClass "product-card"])
    return $ length products
```

### Benefits of This Implementation

1. **Complete Product Coverage**: Now captures all products on the page, not just initially loaded ones
2. **Dynamic Detection**: Automatically detects when all products have been loaded
3. **Efficient**: Stops scrolling as soon as no new products are detected
4. **Robust**: Uses proper WebDriver integration with JavaScript execution
5. **Transparent**: Provides logging to show scrolling progress and product counts

### Dependencies Used

- **WebDriver**: For browser automation and JavaScript execution
- **Control.Concurrent**: For `threadDelay` to wait for content loading
- **Scalpel**: For HTML parsing and product counting

## Usage

When the enhanced nike-scraper runs:

1. **Initialization**: Opens the Nike products page
2. **Infinite Scrolling**: Automatically scrolls and loads all products
3. **Progress Logging**: Shows current product counts and scrolling status
4. **Final Scraping**: Scrapes all products from the fully loaded page
5. **Results**: Outputs all discovered products

Example output:
```
Starting Nike scraper with infinite scrolling support...
Page loaded, starting infinite scroll...
Initial product count: 24
Current product count: 24
After scroll product count: 48
New products loaded, continuing to scroll...
Current product count: 48
After scroll product count: 48
No new products loaded, scrolling complete.
Getting final HTML source...
Successfully scraped 48 products.
```

## Technical Notes

- **JavaScript Execution**: Uses the approach suggested in the GitHub issue with `executeJS`
- **WebDriver Integration**: Properly integrated with the existing WebDriver setup
- **CSS Selectors**: Maintains compatibility with existing product card selectors
- **Performance**: Optimized to minimize unnecessary scrolling while ensuring complete coverage

## Verification

The enhancement addresses the specific requirements mentioned in the GitHub issue:
- ✅ Handles infinite scrolling pages
- ✅ Uses JavaScript execution via WebDriver's `executeJS`
- ✅ Scrolls to bottom until no new products are loaded
- ✅ Uses `window.scrollTo(0, document.body.scrollHeight);` as suggested
- ✅ Maintains existing scraping functionality

This implementation ensures that the Nike scraper now successfully captures all products from pages that use infinite scrolling, significantly improving the completeness and reliability of the scraping results.