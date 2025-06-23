{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper (
    scrapeProducts
) where

import Text.HTML.Scalpel
import Nike.Data
import Data.Maybe (fromMaybe)

scrapeProducts :: Scraper String [Product]
scrapeProducts = chroots ("div" @: [hasClass "product-card"]) $ do
    name <- text ("div" @: [hasClass "product-card__title"])
    subtitle <- text ("div" @: [hasClass "product-card__subtitle"]) <|> return ""
    price <- text ("div" @: [hasClass "product-price"])
    variant <- text ("div" @: [hasClass "product-card__product-count"]) <|> return ""
    imageUrl <- attr "src" ("img" @: [hasClass "product-card__hero-image"]) <|> return ""
    
    -- Extract product URL - try multiple strategies
    -- 1. Look for href in current context
    -- 2. Look for href in any descendant link
    productPageUrl <- (attr "href" ("a") <|> 
                      attr "href" ("a" @: [hasClass "product-card__link-overlay"]) <|>
                      return "") 
    
    return $ Product name subtitle price variant productPageUrl imageUrl 