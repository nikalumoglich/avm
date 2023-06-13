module Handlers.OrdersHandler
    ( listOrdersByUser
    , createOrder
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class

import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Model.Order as Order
import qualified Model.Session as Session
import qualified Transport.OrderCreatedResponse as OrderCreatedResponse
import qualified Transport.OrderResponse as OrderResponse

import Errors ( invalidJsonError, invalidSessionError )

invalidJsonResponse :: ActionT TL.Text IO ()
invalidJsonResponse = status badRequest400 >> json invalidJsonError
unauthorizedResponse :: ActionT TL.Text IO ()
unauthorizedResponse = status unauthorized401 >> json invalidSessionError

listOrdersByUser secret sessionTime bucket conn = HandlersCommons.handleLoggedRequest secret sessionTime conn "userLevel" unauthorizedResponse (\session -> do
                orders <- liftIO (Order.listOrdersByUserId conn bucket (Session.userId session))
                json (map OrderResponse.orderToResponse orders)
                )

createOrder secret sessionTime bucket conn = HandlersCommons.handleLoggedJsonRequest secret sessionTime conn "userLevel" invalidJsonResponse unauthorizedResponse (\createOrderRequest session -> do
                orderId <- liftIO (Order.saveOrder conn bucket (Session.userId session) createOrderRequest)
                json OrderCreatedResponse.OrderCreatedResponse { OrderCreatedResponse.orderId = orderId }
                )