{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper (
    scrapeProducts
) where

import Text.HTML.Scalpel
import Nike.Data

-- Scrape products using multiple possible selectors for better compatibility
scrapeProducts :: Scraper String [Product]
scrapeProducts = do
    -- Try modern Nike product card selectors first
    modernProducts <- chroots ("div" @: [hasClass "product-card"]) scrapeProductCard
    -- Try alternative selectors if first attempt yields empty results  
    altProducts <- chroots ("article" @: [hasClass "product-card"]) scrapeProductCard
    -- Try generic card selectors as fallback
    genericProducts <- chroots ("div" @: ["data-testid" @= "product-card"]) scrapeProductCard
    -- Try CSS grid item selectors
    gridProducts <- chroots ("div" @: [hasClass "grid-item"]) scrapeProductCard
    
    -- Return the first non-empty result
    let allProducts = [modernProducts, altProducts, genericProducts, gridProducts]
    return $ head $ filter (not . null) allProducts ++ [[]]

-- Helper function to scrape individual product card
scrapeProductCard :: Scraper String Product
scrapeProductCard = do
    name <- text ("div" @: [hasClass "product-card__title"]) 
        <|> text ("h3" @: [hasClass "product-card__title"])
        <|> text (".product-title")
        <|> text ("h2")
        <|> text ("h3")
        <|> text ("div" @: [hasClass "title"])
        
    price <- text ("div" @: [hasClass "product-price"]) 
        <|> text ("span" @: [hasClass "price"])
        <|> text (".product-price")
        <|> text ("div" @: [hasClass "price"])
        <|> text ("[data-testid='price']")
        
    imageUrl <- attr "src" ("img" @: [hasClass "product-card__hero-image"])
        <|> attr "src" ("img" @: [hasClass "product-image"])
        <|> attr "src" ("img")
        <|> return "no-image"
        
    return $ Product name price imageUrl 