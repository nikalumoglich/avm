{-# LANGUAGE OverloadedStrings #-}

module ImageSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Database.MySQL.Simple
import Control.Monad
import Model.Image


cleanDb :: Connection -> IO ()
cleanDb dbConn =
  execute dbConn "TRUNCATE TABLE images" ()
  >> execute dbConn "TRUNCATE TABLE products_images" ()
  >> execute dbConn "TRUNCATE TABLE dimensions_images" ()
  >> return ()

suiteSpec :: Connection -> SpecWith ()
suiteSpec dbConn = do
  describe "ImageSpec" $ do

    it "should get images by product" $ do
      cleanDb dbConn
      void (execute dbConn "INSERT INTO images (`key`) VALUES ('http://imageurl.com')" ())
      void (execute dbConn "INSERT INTO products_images (product_id, image_id) VALUES (1, 1)" ())
      void (execute dbConn "INSERT INTO products () VALUES ()" ())
      images <- getImagesByProductId dbConn 1
      head images `shouldBe` Image { imageId = 1, url = "http://imageurl.com" }

    it "should get images by dimension" $ do
      cleanDb dbConn
      void (execute dbConn "INSERT INTO images (`key`) VALUES ('http://imageurl.com')" ())
      void (execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 1)" ())
      void (execute dbConn "INSERT INTO dimensions () VALUES ()" ())
      images <- getImagesByDimensionId dbConn 1
      head images `shouldBe` Image { imageId = 1, url = "http://imageurl.com" }

    it "should show image" $ do
      cleanDb dbConn
      void (execute dbConn "INSERT INTO images (`key`) VALUES ('http://imageurl.com')" ())
      void (execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 1)" ())
      void (execute dbConn "INSERT INTO dimensions () VALUES ()" ())
      images <- getImagesByDimensionId dbConn 1
      head images `shouldBe` Image { imageId = 1, url = "http://imageurl.com" }
      show (head images) `shouldBe` "Image {imageId = 1, url = \"http://imageurl.com\"}"
      imageId (head images) `shouldBe` 1
      url (head images) `shouldBe` "http://imageurl.com"

