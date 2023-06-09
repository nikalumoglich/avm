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
    , interactions
    , getOrderById
    , saveOrder
    , listOrdersByUserId
    , saveOrderInteraction
    , Order ( Order, OrderNotFound )
    , OrderDimension ( OrderDimension )
    , OrderInteractions (OrderInteractions)
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
import qualified Model.Image as Image
import qualified Transport.CreateOrderRequest as CreateOrderRequest
import qualified Transport.CreateOrderInteractionRequest as CreateOrderInteractionRequest
import qualified Controller.ProductController as ProductController

data Order = Order
  { orderId :: Int
  , user :: User.User
  , product :: Product.Product
  , openingDate :: UTCTime
  , closingDate :: Maybe UTCTime
  , price :: Int
  , dimensions :: [OrderDimension]
  , interactions :: [OrderInteractions]
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

data OrderInteractions = OrderInteractions
  { interactionId :: Int
  , authorId :: Int
  , text :: String
  , images :: [Image.Image]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON OrderInteractions
instance Aeson.ToJSON OrderInteractions

getOrderDimensionsForOrder :: Connection -> Int -> IO [OrderDimension]
getOrderDimensionsForOrder conn orderId = do
    rows <- query conn "SELECT orders_dimensions.dimension_id, dimensions.name, orders_dimensions.value FROM orders_dimensions INNER JOIN dimensions on orders_dimensions.dimension_id = dimensions.id WHERE orders_dimensions.order_id = ?" (Only orderId :: Only Int)
    return (map (\(dimensionId, dimensionName, orderDimensionValue) -> OrderDimension { dimensionId = dimensionId, dimensionName = dimensionName, value = orderDimensionValue }) rows)

getInteractionsForOrder :: Connection -> String -> Int -> IO [OrderInteractions]
getInteractionsForOrder conn bucket orderId = do
    rows <- query conn "SELECT order_interactions.id, order_interactions.author_id, order_interactions.text, order_interactions.created_at FROM order_interactions WHERE order_interactions.order_id = ?" (Only orderId :: Only Int)
    mapM (\(interactionId, authorId, text, createdAt) -> do
        rows' <- query conn "SELECT order_interactions_images.image_id FROM order_interactions_images WHERE order_interaction_id = ?" (Only interactionId :: Only Int)
        let imageIds = map (\(Only imageId) -> imageId) rows'
        images <- mapM (Image.getImagesById conn bucket) imageIds
        return OrderInteractions { interactionId = interactionId, authorId = authorId, text = text, images = images, createdAt = createdAt }) rows

saveOrderDimensions :: Foldable t => Connection -> Int -> t Dimension.Dimension -> [ProductController.DimensionValue] -> IO ()
saveOrderDimensions conn orderId dimensions values = do
    let valuesMap = Map.fromList (map (\value -> (ProductController.dimensionId value, ProductController.value value)) values)
    mapM_ (\dimension -> do
        let dimensionId = Dimension.dimensionId dimension
        let value = valuesMap Map.! dimensionId
        execute conn "INSERT INTO orders_dimensions (order_id, dimension_id, value) values (?, ?, ?)" (orderId :: Int, dimensionId :: Int, value :: Int)
        ) dimensions

getOrderById :: Connection -> String -> Int -> IO Order
getOrderById conn bucket orderId = do
    rows <- query conn "SELECT * FROM orders WHERE id = ?" (Only orderId :: Only Int)
    case rows of
        [] -> return OrderNotFound
        (orderId, userId, productId, openingDate, closingDate, price):_ -> do
            user <- User.getUserById conn userId
            product <- Product.getProductById conn bucket productId
            orderDimensions <- getOrderDimensionsForOrder conn orderId
            interactions <- getInteractionsForOrder conn bucket orderId
            let order = Order { orderId = orderId, user = user, product = product, openingDate = openingDate, closingDate = closingDate, price = price, dimensions = orderDimensions, interactions = interactions }
            return order

saveOrder :: Connection -> String -> Int -> CreateOrderRequest.CreateOrderRequest -> IO Int
saveOrder conn bucket userId createOrderRequest = do
    product <- Product.getProductById conn bucket (CreateOrderRequest.productId createOrderRequest)
    price <- ProductController.calculatePrice product (CreateOrderRequest.dimensionValues createOrderRequest)
    currentDatetime <- getCurrentTime
    _ <- execute conn "INSERT INTO orders (user_id, product_id, opening_date, closing_date, price) values (?, ?, ?, ?, ?)" (userId :: Int, Product.productId product :: Int, currentDatetime, Nothing :: Maybe UTCTime, price :: Int)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    _ <- saveOrderDimensions conn lastReturnedId (Product.dimensions product) (CreateOrderRequest.dimensionValues createOrderRequest)
    return lastReturnedId

listOrdersByUserId :: Connection -> String -> Int -> IO [Order]
listOrdersByUserId conn bucket userId = do
    rows <- query conn "SELECT * FROM orders WHERE user_id = ?" (Only userId :: Only Int)
    mapM (\(orderId, userId, productId, openingDate, closingDate, price) -> do
        user <- User.getUserById conn userId
        product <- Product.getProductById conn bucket productId
        orderDimensions <- getOrderDimensionsForOrder conn orderId
        interactions <- getInteractionsForOrder conn bucket orderId
        return Order { orderId = orderId, user = user, product = product, openingDate = openingDate, closingDate = closingDate, price = price, dimensions = orderDimensions, interactions = interactions }
        ) rows

saveOrderInteractionImages :: Foldable t => Connection -> Int -> t Int -> IO ()
saveOrderInteractionImages conn interactionId imageIds = do
    mapM_ (\imageId -> do
        execute conn "INSERT INTO order_interactions_images (order_interaction_id, image_id) values (?, ?)" (interactionId :: Int, imageId :: Int)
        ) imageIds

saveOrderInteraction :: Connection -> Int -> CreateOrderInteractionRequest.CreateOrderInteractionRequest -> IO Int
saveOrderInteraction conn userId createOrderInteractionRequest = do
    let orderId = CreateOrderInteractionRequest.orderId createOrderInteractionRequest
    currentDatetime <- getCurrentTime
    _ <- execute conn "INSERT INTO order_interactions (order_id, author_id, text, created_at) values (?, ?, ?, ?)" (orderId, userId :: Int, CreateOrderInteractionRequest.text createOrderInteractionRequest, currentDatetime)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    _ <- saveOrderInteractionImages conn lastReturnedId (CreateOrderInteractionRequest.imageIds createOrderInteractionRequest)
    return lastReturnedId

