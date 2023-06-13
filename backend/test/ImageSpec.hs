{-# LANGUAGE OverloadedStrings #-}

module ImageSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Database.MySQL.Simple
import Control.Monad
import qualified Data.Text as T
import Model.Image

cleanDb :: Connection -> IO ()
cleanDb dbConn =
  execute dbConn "TRUNCATE TABLE images" ()
  >> execute dbConn "TRUNCATE TABLE products_images" ()
  >> execute dbConn "TRUNCATE TABLE dimensions_images" ()
  >> return ()

suiteSpec :: Connection -> String -> SpecWith ()
suiteSpec dbConn bucket = do
  describe "ImageSpec" $ do

    it "should get images by product" $ do
      cleanDb dbConn
      void (execute dbConn "INSERT INTO images (`key`) VALUES ('imageKey1')" ())
      void (execute dbConn "INSERT INTO products_images (product_id, image_id) VALUES (1, 1)" ())
      void (execute dbConn "INSERT INTO products () VALUES ()" ())
      images <- getImagesByProductId dbConn bucket 1
      let imageUrl = T.pack (url (head images))
      let predicate1 = T.isInfixOf (T.pack ("https://" ++ bucket ++ ".s3")) imageUrl
      let predicate2 = T.isInfixOf "X-Amz-Expires=60" imageUrl
      let predicate3 = T.isInfixOf "imageKey1" imageUrl
      predicate1 && predicate2 && predicate3 `shouldBe` True

    it "should get images by dimension" $ do
      cleanDb dbConn
      void (execute dbConn "INSERT INTO images (`key`) VALUES ('imageKey2')" ())
      void (execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 1)" ())
      void (execute dbConn "INSERT INTO dimensions () VALUES ()" ())
      images <- getImagesByDimensionId dbConn bucket 1
      let imageUrl = T.pack (url (head images))
      let predicate1 = T.isInfixOf (T.pack ("https://" ++ bucket ++ ".s3")) imageUrl
      let predicate2 = T.isInfixOf "X-Amz-Expires=60" imageUrl
      let predicate3 = T.isInfixOf "imageKey2" imageUrl
      predicate1 && predicate2 && predicate3 `shouldBe` True

    it "should show image" $ do
      cleanDb dbConn
      void (execute dbConn "INSERT INTO images (`key`) VALUES ('imageKey3')" ())
      void (execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 1)" ())
      void (execute dbConn "INSERT INTO dimensions () VALUES ()" ())
      images <- getImagesByDimensionId dbConn bucket 1
      let imageUrl = T.pack (url (head images))
      let predicate1 = T.isInfixOf (T.pack ("https://" ++ bucket ++ ".s3")) imageUrl
      let predicate2 = T.isInfixOf "X-Amz-Expires=60" imageUrl
      let predicate3 = T.isInfixOf "imageKey3" imageUrl
      predicate1 && predicate2 && predicate3 `shouldBe` True
      let predicate4 = T.isInfixOf (T.pack ("Image {imageId = 1, url = \"https://" ++ bucket ++ ".s3")) (T.pack (show (head images)))
      predicate4 `shouldBe` True
      imageId (head images) `shouldBe` 1
      let predicate5 = T.isInfixOf  (T.pack ("https://" ++ bucket ++ ".s3")) (T.pack (url (head images)))
      predicate5 `shouldBe` True

