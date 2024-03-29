{-# LANGUAGE OverloadedStrings #-}

module Handlers.OrdersHandler
    ( getOrderById
    , listOrdersByUser
    , createOrder
    , createOrderInteraction
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
import qualified Transport.OrderInteractionCreatedResponse as OrderInteractionCreatedResponse

import Errors ( invalidJsonError, invalidSessionError )

invalidJsonResponse = status badRequest400 >> json invalidJsonError
unauthorizedResponse = status unauthorized401 >> json invalidSessionError

getOrderById secret sessionTime bucket conn = HandlersCommons.handleLoggedRequest secret sessionTime conn "userLevel" unauthorizedResponse (\_ -> do
                orderId <- param "orderId"
                order <- liftIO (Order.getOrderById conn bucket orderId)
                json (OrderResponse.orderToResponse order)
                )

listOrdersByUser secret sessionTime bucket conn = HandlersCommons.handleLoggedRequest secret sessionTime conn "userLevel" unauthorizedResponse (\session -> do
                orders <- liftIO (Order.listOrdersByUserId conn bucket (Session.userId session))
                json (map OrderResponse.orderToResponse orders)
                )

createOrder secret sessionTime bucket conn = HandlersCommons.handleLoggedJsonRequest secret sessionTime conn "userLevel" invalidJsonResponse unauthorizedResponse (\createOrderRequest session -> do
                orderId <- liftIO (Order.saveOrder conn bucket (Session.userId session) createOrderRequest)
                json OrderCreatedResponse.OrderCreatedResponse { OrderCreatedResponse.orderId = orderId }
                )

createOrderInteraction secret sessionTime conn = HandlersCommons.handleLoggedJsonRequest secret sessionTime conn "userLevel" invalidJsonResponse unauthorizedResponse (\createOrderInteractionRequest session -> do
                let authorId = Session.userId session
                orderInteractionId <-liftIO (Order.saveOrderInteraction conn authorId createOrderInteractionRequest)
                json OrderInteractionCreatedResponse.OrderInteractionCreatedResponse { OrderInteractionCreatedResponse.orderInteractionId = orderInteractionId }
                )