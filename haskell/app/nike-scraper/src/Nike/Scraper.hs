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
    price <- text ("div" @: [hasClass "product-price"])
    imageUrl <- attr "src" ("img" @: [hasClass "product-card__hero-image"])
    
    -- Extract new fields with fallbacks for missing data
    subtitle <- fromMaybe "" <$> optional (text ("div" @: [hasClass "product-card__subtitle"]))
    variant <- fromMaybe "" <$> optional (text ("div" @: [hasClass "product-card__product-count"]))
    
    -- Try multiple approaches to extract product URL
    productUrl <- fromMaybe "" <$> optional (
        attr "href" ("a" @: [hasClass "product-card__link-overlay"]) <|>
        attr "href" "a" <|>  -- Fallback to any anchor tag
        attr "data-href" ("div" @: [hasClass "product-card"])  -- Some sites use data-href
        )
    
    return $ Product name price imageUrl subtitle variant productUrl 