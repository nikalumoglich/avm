{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.Order
    ( orderId
    , user
    , product
    , openingDate
    , closingDate
    , price
    , getOrderById
    , saveOrder
    , listOrdersByUserId
    , Order ( Order, OrderNotFound )
    ) where

import Data.Time
import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson
import qualified Model.User as User
import qualified Model.Product as Product
import qualified Transport.CreateOrderRequest as CreateOrderRequest
import qualified Controller.ProductController as ProductController
import Prelude hiding (product)

data Order = Order
  { orderId :: Int
  , user :: User.User
  , product :: Product.Product
  , openingDate :: UTCTime
  , closingDate :: Maybe UTCTime
  , price :: Int
  } | OrderNotFound
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Order
instance Aeson.ToJSON Order

getOrderById :: Connection -> Int -> IO Order
getOrderById conn orderId = do
    rows <- query conn "SELECT * FROM orders WHERE id = ?" (Only orderId :: Only Int)
    case rows of
        [] -> return OrderNotFound
        (orderId, userId, productId, openingDate, closingDate, price):_ -> do
            user <- User.getUserById conn userId
            product <- Product.getProductById conn productId
            let order = Order { orderId = orderId, user = user, product = product, openingDate = openingDate, closingDate = closingDate, price = price }
            return order

saveOrder :: Result b => Connection -> Int -> CreateOrderRequest.CreateOrderRequest -> IO b
saveOrder conn userId createOrderRequest = do
    product <- Product.getProductById conn (CreateOrderRequest.productId createOrderRequest)
    price <- ProductController.calculatePrice product (CreateOrderRequest.dimensionValues createOrderRequest)
    currentDatetime <- getCurrentTime
    _ <- execute conn "INSERT INTO orders (user_id, product_id, opening_date, closing_date, price) values (?, ?, ?, ?, ?)" (userId :: Int, Product.productId product :: Int, currentDatetime, Nothing :: Maybe UTCTime, price :: Int)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    return lastReturnedId

listOrdersByUserId :: Connection -> Int -> IO [Order]
listOrdersByUserId conn userId = do
    rows <- query conn "SELECT * FROM orders WHERE user_id = ?" (Only userId :: Only Int)
    mapM (\(orderId, userId, productId, openingDate, closingDate, price) -> do
        user <- User.getUserById conn userId
        product <- Product.getProductById conn productId
        return Order { orderId = orderId, user = user, product = product, openingDate = openingDate, closingDate = closingDate, price = price }
        ) rows