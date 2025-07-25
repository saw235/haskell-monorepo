{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper
  ( scrapeProducts,
  )
where

import Nike.Data
import Text.HTML.Scalpel

scrapeProducts :: Scraper String [Product]
scrapeProducts = chroots ("div" @: [hasClass "product-card"]) $ do
  name <- text ("div" @: [hasClass "product-card__title"])
  price <- text ("div" @: [hasClass "product-price"])
  imageUrl <- attr "src" ("img" @: [hasClass "product-card__hero-image"])
  subtitle <- text ("div" @: [hasClass "product-card__subtitle"])
  variant <- text ("div" @: [hasClass "product-card__product-count"])
  productUrl <- attr "href" ("a" @: [hasClass "product-card__link-overlay"])
  return $ Product name price imageUrl subtitle variant productUrl
