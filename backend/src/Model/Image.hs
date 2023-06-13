{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.Image
    ( imageId
    , url
    , getImagesByProductId
    , getImagesByDimensionId
    , saveImage
    , Image ( Image )
    ) where

import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Controller.AwsUtils as AwsUtils
import qualified Amazonka.S3 as S3

data Image = Image
  { imageId :: Int
  , url :: String
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Image
instance Aeson.ToJSON Image

rowToImage :: String -> (Int, String) -> IO Image
rowToImage bucket (imageId, key) = do
    url <- AwsUtils.getPresignedURL (S3.BucketName (T.pack bucket)) (S3.ObjectKey (T.pack key))
    return (Image { imageId = imageId, url = url })

getImagesByProductId :: Connection -> String -> Int -> IO [Image]
getImagesByProductId conn bucket productId = do
    rows <- query conn "SELECT images.* FROM products INNER JOIN products_images ON products.id = products_images.product_id INNER JOIN images ON products_images.image_id = images.id WHERE products.id = ?" (Only productId :: Only Int)
    mapM (rowToImage bucket) rows

getImagesByDimensionId :: Connection -> String -> Int -> IO [Image]
getImagesByDimensionId conn bucket dimensionId = do
    rows <- query conn "SELECT images.* FROM dimensions INNER JOIN dimensions_images ON dimensions.id = dimensions_images.dimension_id INNER JOIN images ON dimensions_images.image_id = images.id WHERE dimensions.id = ?" (Only dimensionId :: Only Int)
    mapM (rowToImage bucket) rows

saveImage :: Connection -> String -> String -> IO Image
saveImage conn bucket key = do
    _ <- execute conn "INSERT INTO images (`key`) values (?)" (Only key)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    url <- AwsUtils.getPresignedURL (S3.BucketName (T.pack bucket)) (S3.ObjectKey (T.pack key))
    return Image { imageId = lastReturnedId, url = url }
