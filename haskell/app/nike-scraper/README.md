# Nike Scraper

A Haskell-based web scraper for extracting product information from Nike's website.

## Features

The scraper extracts comprehensive product information from Nike product cards:

- **Product Name**: The main product title (e.g., "Air Jordan 5 Retro 'Grape'")
- **Product Subtitle**: Category information (e.g., "Men's Shoes") 
- **Product Price**: Current pricing information (e.g., "$215")
- **Product Variant**: Available color/variant information (e.g., "1 Color")
- **Product URL**: Direct link to the product detail page
- **Product Image**: URL of the product hero image

## Prerequisites

### Chrome Browser
To run the Nike scraper, you need to have Google Chrome installed. On Ubuntu/Debian systems:
```bash
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

## Building and Running

Build using Bazel:
```bash
bazel build //haskell/app/nike-scraper:nike-scraper
bazel run //haskell/app/nike-scraper:nike-scraper
```

## Implementation Details

### Data Structure
The `Product` data type captures all extracted information:
```haskell
data Product = Product {
    productName     :: String,
    productSubtitle :: String, 
    productPrice    :: String,
    productVariant  :: String,
    productUrl      :: String,
    productImage    :: String
} deriving (Show, Eq)
```

### CSS Selectors Used
- **Product Name**: `.product-card__title`
- **Product Subtitle**: `.product-card__subtitle`  
- **Product Price**: `.product-price`
- **Product Variant**: `.product-card__product-count`
- **Product Image**: `.product-card__hero-image[src]`
- **Product URL**: `a[href]` (from parent or descendant link elements)

### Error Handling
The scraper gracefully handles missing fields by using the `<|>` operator to provide empty string defaults for optional fields like subtitle and variant information.

## Recent Updates

**Enhanced Product Information Extraction (Issue #4)**
- Added support for extracting product subtitles (e.g., "Men's Shoes")
- Added support for extracting variant information (e.g., "1 Color") 
- Added support for extracting product page URLs
- Improved error handling for missing optional fields