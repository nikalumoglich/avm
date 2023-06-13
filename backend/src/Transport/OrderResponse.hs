{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.OrderResponse
    ( orderToResponse
    , OrderResponse ( OrderResponse )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Data.Time
import qualified Model.Order as Order
import qualified Model.User as User
import qualified Model.Product as Product

data OrderResponse = OrderResponse
  { orderId :: Int
  , userId :: Int
  , productId :: Int
  , openingDate :: UTCTime
  , closingDate :: Maybe UTCTime
  , price :: Int
  , dimensions :: [Order.OrderDimension]
  , interactions :: [Order.OrderInteractions]
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON OrderResponse

orderToResponse :: Order.Order -> OrderResponse
orderToResponse order = OrderResponse {
  orderId = Order.orderId order,
  userId = User.userId $ Order.user order,
  productId = Product.productId $ Order.product order,
  openingDate = Order.openingDate order,
  closingDate = Order.closingDate order,
  price = Order.price order,
  dimensions = Order.dimensions order,
  interactions = Order.interactions order
  }
