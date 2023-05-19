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
    , dimensions
    , getOrderById
    , saveOrder
    , listOrdersByUserId
    , Order ( Order, OrderNotFound )
    , OrderDimension ( OrderDimension )
    ) where

import Prelude hiding (product)
import Data.Time
import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Model.User as User
import qualified Model.Product as Product
import qualified Model.Dimension as Dimension
import qualified Transport.CreateOrderRequest as CreateOrderRequest
import qualified Controller.ProductController as ProductController

data Order = Order
  { orderId :: Int
  , user :: User.User
  , product :: Product.Product
  , openingDate :: UTCTime
  , closingDate :: Maybe UTCTime
  , price :: Int
  , dimensions :: [OrderDimension]
  } | OrderNotFound
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Order
instance Aeson.ToJSON Order

data OrderDimension = OrderDimension
  { dimensionId :: Int
  , dimensionName :: String
  , value :: Int
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON OrderDimension
instance Aeson.ToJSON OrderDimension

getOrderDimensionsForOrder :: Connection -> Int -> IO [OrderDimension]
getOrderDimensionsForOrder conn orderId = do
    rows <- query conn "SELECT orders_dimensions.dimension_id, dimensions.name, orders_dimensions.value from orders_dimensions INNER JOIN dimensions on orders_dimensions.dimension_id  = dimensions.id  WHERE  orders_dimensions.order_id = ?" (Only orderId :: Only Int)
    return (map (\(dimensionId, dimensionName, orderDimensionValue) -> OrderDimension { dimensionId = dimensionId, dimensionName = dimensionName, value = orderDimensionValue }) rows)


saveOrderDimensions :: Foldable t => Connection -> Int -> t Dimension.Dimension -> [ProductController.DimensionValue] -> IO ()
saveOrderDimensions conn orderId dimensions values = do
    let valuesMap = Map.fromList (map (\value -> (ProductController.dimensionId value, ProductController.value value)) values)
    mapM_ (\dimension -> do
        let dimensionId = Dimension.dimensionId dimension
        let value = valuesMap Map.! dimensionId
        execute conn "INSERT INTO orders_dimensions (order_id, dimension_id, value) values (?, ?, ?)" (orderId :: Int, dimensionId :: Int, value :: Int)
        ) dimensions

getOrderById :: Connection -> Int -> IO Order
getOrderById conn orderId = do
    rows <- query conn "SELECT * FROM orders WHERE id = ?" (Only orderId :: Only Int)
    case rows of
        [] -> return OrderNotFound
        (orderId, userId, productId, openingDate, closingDate, price):_ -> do
            user <- User.getUserById conn userId
            product <- Product.getProductById conn productId
            orderDimensions <- getOrderDimensionsForOrder conn orderId
            let order = Order { orderId = orderId, user = user, product = product, openingDate = openingDate, closingDate = closingDate, price = price, dimensions = orderDimensions }
            return order

saveOrder :: Connection -> Int -> CreateOrderRequest.CreateOrderRequest -> IO Int
saveOrder conn userId createOrderRequest = do
    product <- Product.getProductById conn (CreateOrderRequest.productId createOrderRequest)
    price <- ProductController.calculatePrice product (CreateOrderRequest.dimensionValues createOrderRequest)
    currentDatetime <- getCurrentTime
    _ <- execute conn "INSERT INTO orders (user_id, product_id, opening_date, closing_date, price) values (?, ?, ?, ?, ?)" (userId :: Int, Product.productId product :: Int, currentDatetime, Nothing :: Maybe UTCTime, price :: Int)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    _ <- saveOrderDimensions conn lastReturnedId (Product.dimensions product) (CreateOrderRequest.dimensionValues createOrderRequest)
    return lastReturnedId

listOrdersByUserId :: Connection -> Int -> IO [Order]
listOrdersByUserId conn userId = do
    rows <- query conn "SELECT * FROM orders WHERE user_id = ?" (Only userId :: Only Int)
    mapM (\(orderId, userId, productId, openingDate, closingDate, price) -> do
        user <- User.getUserById conn userId
        product <- Product.getProductById conn productId
        orderDimensions <- getOrderDimensionsForOrder conn orderId
        return Order { orderId = orderId, user = user, product = product, openingDate = openingDate, closingDate = closingDate, price = price, dimensions = orderDimensions }
        ) rows