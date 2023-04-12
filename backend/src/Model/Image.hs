{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.Image
    ( imageId
    , url
    , getImagesByProductId
    , getImagesByDimensionId
    , Image ( Image )
    ) where

import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson

data Image = Image
  { imageId :: Int
  , url :: String
  } 
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Image
instance Aeson.ToJSON Image

rowToImage :: (Int, String) -> Image
rowToImage (imageId, url) = Image { imageId = imageId, url = url }

getImagesByProductId :: Connection -> Int -> IO [Image]
getImagesByProductId conn productId = do
    rows <- query conn "SELECT images.* FROM products INNER JOIN products_images ON products.id = products_images.product_id INNER JOIN images ON products_images.image_id = images.id WHERE products.id = ?" (Only productId :: Only Int)
    return (map rowToImage rows)

getImagesByDimensionId :: Connection -> Int -> IO [Image]
getImagesByDimensionId conn dimensionId = do
    rows <- query conn "SELECT images.* FROM dimensions INNER JOIN dimensions_images ON dimensions.id = dimensions_images.dimension_id INNER JOIN images ON dimensions_images.image_id = images.id WHERE dimensions.id = ?" (Only dimensionId :: Only Int)
    return (map rowToImage rows)