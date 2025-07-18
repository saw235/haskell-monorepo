{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Adidas.Data (
    Product(..),
    ApiResponse(..),
    ProductContainer(..),
    ApiProduct(..),
    Price(..)
) where

import GHC.Generics
import Data.Aeson

data Product = Product {
    productName     :: String,
    productPrice    :: String,
    productImage    :: String,
    productSubtitle :: String,
    productVariant  :: String,
    productUrl      :: String
} deriving (Show, Eq, Generic)

instance ToJSON Product

data ApiResponse = ApiResponse {
    objects :: [ProductContainer]
} deriving (Show, Generic)

instance FromJSON ApiResponse

data ProductContainer = ProductContainer {
    product :: ApiProduct
} deriving (Show, Generic)

instance FromJSON ProductContainer

data ApiProduct = ApiProduct {
    title :: String,
    price :: Price
} deriving (Show, Generic)

instance FromJSON ApiProduct

data Price = Price {
    currentPrice :: Double
} deriving (Show, Generic)

instance FromJSON Price 