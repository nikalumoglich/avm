{-# LANGUAGE OverloadedStrings #-}

module DimensionSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Database.MySQL.Simple
import Control.Monad
import Model.Dimension


cleanDb :: Connection -> IO ()
cleanDb dbConn = 
  execute dbConn "TRUNCATE TABLE dimensions" ()
  >> execute dbConn "TRUNCATE TABLE images" ()
  >> return ()

suiteSpec :: Connection -> SpecWith ()
suiteSpec dbConn = do
  describe "DimensionSpec" $ do

    it "should get dimensions by product" $ do
      cleanDb dbConn
      void (execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'dimension name', '{dimension}')" ())
      dimensions <- getDimensionsByProductId dbConn 1
      head dimensions `shouldBe` Dimension { dimensionId = 1, productId = 1, name = "dimension name", symbol = "{dimension}", images = [] }
