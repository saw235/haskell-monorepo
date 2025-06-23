{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper (
    scrapeProducts
) where

import Text.HTML.Scalpel
import Nike.Data
import Control.Applicative ((<|>))

scrapeProducts :: Scraper String [Product]
scrapeProducts = do
    -- Try multiple possible selectors for Nike product cards
    products1 <- chroots ("div" @: [hasClass "product-card"]) scrapeProduct
    products2 <- chroots ("div" @: [hasClass "card"]) scrapeProduct
    products3 <- chroots ("div" @: [hasClass "product-tile"]) scrapeProduct
    products4 <- chroots ("div" @: [hasClass "product"]) scrapeProduct
    
    -- Return the first non-empty result
    return $ if not (null products1) then products1
             else if not (null products2) then products2
             else if not (null products3) then products3
             else products4

scrapeProduct :: Scraper String Product
scrapeProduct = do
    -- Try multiple selectors for product name
    name <- (text ("div" @: [hasClass "product-card__title"]) <|>
             text ("h3" @: [hasClass "product-card__title"]) <|>
             text ("div" @: [hasClass "product-card__subtitle"]) <|>
             text ("h3") <|>
             text ("div" @: [hasClass "card-title"]) <|>
             text ("div" @: [hasClass "product-title"]) <|>
             text (".product-card__title") <|>
             text ("[data-testid=product-title]"))
    
    -- Try multiple selectors for product price
    price <- (text ("div" @: [hasClass "product-price"]) <|>
              text ("span" @: [hasClass "product-price"]) <|>
              text ("div" @: [hasClass "price"]) <|>
              text ("span" @: [hasClass "price"]) <|>
              text (".product-price") <|>
              text ("[data-testid=product-price]") <|>
              text ("div" @: [hasClass "product-card__price"]))
    
    -- Try multiple selectors for product image
    imageUrl <- (attr "src" ("img" @: [hasClass "product-card__hero-image"]) <|>
                 attr "src" ("img" @: [hasClass "card-image"]) <|>
                 attr "src" ("img" @: [hasClass "product-image"]) <|>
                 attr "src" "img" <|>
                 attr "data-src" "img")
    
    return $ Product name price (maybe "" id imageUrl) 