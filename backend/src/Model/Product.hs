{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.Product
    ( productId
    , name
    , description
    , priceFormula
    , dimensions
    , images
    , getProductById
    , listProducts
    , Product ( Product, ProductNotFound )
    ) where

import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson
import qualified Model.Dimension as Dimension
import qualified Model.Image as Image

data Product = Product
  { productId :: Int
  , name :: String
  , description :: String
  , priceFormula :: String
  , dimensions :: [Dimension.Dimension]
  , images :: [Image.Image]
  } | ProductNotFound
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Product
instance Aeson.ToJSON Product

getProductById :: Connection -> String -> Int -> IO Product
getProductById conn bucket productId' = do
    rows <- query conn "SELECT * FROM products WHERE id = ?" (Only productId' :: Only Int)
    case rows of
        [] -> return ProductNotFound
        (productId', name, description, priceFormula):_ -> do
            let product = Product { productId = productId', name = name, description = description, priceFormula = priceFormula }
            dimensions <- Dimension.getDimensionsByProductId conn bucket (productId product)
            images <- Image.getImagesByProductId conn bucket (productId product)
            return (product { dimensions = dimensions, images = images })

listProducts :: Connection -> String -> IO [Product]
listProducts conn bucket = do
    rows <- query_ conn "SELECT * FROM products"
    let products = map (\(productId, name, description, priceFormula) -> Product { productId = productId, name = name, description = description, priceFormula = priceFormula }) rows
    mapM (\product -> do
        dimensions <- Dimension.getDimensionsByProductId conn bucket (productId product)
        images <- Image.getImagesByProductId conn bucket (productId product)
        return product { dimensions = dimensions, images = images }
        ) products