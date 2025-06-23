{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper (
    scrapeProducts,
    countProducts
) where

import Text.HTML.Scalpel
import Nike.Data
import qualified Data.Text as T

scrapeProducts :: Scraper String [Product]
scrapeProducts = chroots ("div" @: [hasClass "product-card"]) $ do
    name <- text ("div" @: [hasClass "product-card__title"])
    price <- text ("div" @: [hasClass "product-price"])
    imageUrl <- attr "src" ("img" @: [hasClass "product-card__hero-image"])
    return $ Product name price imageUrl

-- Count the number of product cards on the page
countProducts :: String -> Int
countProducts html = 
    case scrapeStringLike html (length <$> chroots ("div" @: [hasClass "product-card"]) (return ())) of
        Just count -> count
        Nothing    -> 0 