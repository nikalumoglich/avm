{-# LANGUAGE OverloadedStrings #-}

module Handlers.ProductsHandler
    ( getProduct
    , listProducts
    , calculatePrice
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class

import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Model.Product as Product
import qualified Transport.CalculatePriceRequest as CalculatePriceRequest
import qualified Transport.CalculatePriceResponse as CalculatePriceResponse

import Errors ( invalidJsonError, invalidSessionError )

import qualified Controller.ProductController as ProductController


invalidJsonResponse :: ActionT TL.Text IO ()
invalidJsonResponse = status badRequest400 >> json invalidJsonError
unauthorizedResponse :: ActionT TL.Text IO ()
unauthorizedResponse = status unauthorized401 >> json invalidSessionError

getProduct secret sessionTime bucket conn = HandlersCommons.handleLoggedRequest secret sessionTime conn "userLevel" unauthorizedResponse (\_ -> do
    productId <- param "productId"
    product <- liftIO (Product.getProductById conn bucket productId)
    json product
    )

listProducts secret sessionTime bucket conn = HandlersCommons.handleLoggedRequest secret sessionTime conn "userLevel" unauthorizedResponse (\_ -> do
                products <- liftIO (Product.listProducts conn bucket)
                json products
                )

calculatePrice secret sessionTime bucket conn = HandlersCommons.handleLoggedJsonRequest secret sessionTime conn "userLevel" invalidJsonResponse unauthorizedResponse (\calculatePriceRequest _ -> do
                product <- liftIO (Product.getProductById conn bucket (CalculatePriceRequest.productId calculatePriceRequest))
                value <- ProductController.calculatePrice product (CalculatePriceRequest.dimensionValues calculatePriceRequest)
                json CalculatePriceResponse.CalculatePriceResponse { CalculatePriceResponse.value = value }
                )