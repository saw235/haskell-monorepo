{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper (
    scrapeProducts,
    countProducts
) where

import Text.HTML.Scalpel
import Nike.Data

scrapeProducts :: Scraper String [Product]
scrapeProducts = chroots ("div" @: [hasClass "product-card"]) $ do
    name <- text ("div" @: [hasClass "product-card__title"])
    price <- text ("div" @: [hasClass "product-price"])
    imageUrl <- attr "src" ("img" @: [hasClass "product-card__hero-image"])
    return $ Product name price imageUrl

-- Count the number of product cards on the page
countProducts :: Scraper String Int
countProducts = do
    products <- attrs "class" ("div" @: [hasClass "product-card"])
    return $ length products 