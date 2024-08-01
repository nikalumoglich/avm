{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App
    ( app
    , api
    , getEnvOrDefault
    ) where

import Web.Scotty
import Database.MySQL.Simple
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import System.Environment
import Control.Exception
import Control.Monad
import System.IO.Error
import qualified Handlers.SignUpHandler as SignUpHandler
import qualified Handlers.SignInHandler as SignInHandler
import qualified Handlers.LoggedHandler as LoggedHandler
import qualified Handlers.ProductsHandler as ProductsHandler
import qualified Handlers.OrdersHandler as OrdersHandler
import qualified Handlers.ImagesHandler as ImagesHandler
import qualified Handlers.AccountRecoveryHandler as AccountRecoveryHandler

getEnvOrDefault :: String -> String -> IO String
getEnvOrDefault name defaultValue = getEnv name `catch` handleIsDoesNotExistError defaultValue

handleIsDoesNotExistError :: String -> System.IO.Error.IOError -> IO String
handleIsDoesNotExistError defaultValue _ = return defaultValue

{-
app :: Bool -> IO ()
app shouldStart = do
    host <- getEnvOrDefault "DB_HOST" "127.0.0.1"
    database <- getEnvOrDefault "DB_NAME" "avm"
    user <- getEnvOrDefault "DB_USER" "haskelluser"
    password <- getEnvOrDefault "DB_PASSWORD" "haskellpassword"
    secret <- getEnvOrDefault "JWT_SECRET" "secret"
    port <- getEnvOrDefault "AVM_PORT" "3000"
    sessionTime' <- getEnvOrDefault "SESSION_TIME" "900"
    let sessionTime = read sessionTime'
    bucket <- getEnvOrDefault "AWS_S3_BUCKET" "tiozao-avm"
    version <- getEnvOrDefault "AVM_VERSION" "version not defined"

    putStrLn ("AVM running on port " ++ port ++ " - version: " ++ version)

    when shouldStart $ api host database user password secret sessionTime bucket >>= run (read port)
-}

app :: Bool -> IO ()
app shouldStart = scotty 3000 $
    get "/" $ do
        html $ mconcat ["<h1>Scotty, beam me up!</h1>"]

api :: String -> String -> String -> String -> String -> Int -> String -> IO Application
api host database user password secret sessionTime bucket = do
    dbConn <- connect (defaultConnectInfo { connectHost = host, connectUser = user, connectPassword = password, connectDatabase = database })

    scottyApp $ do

        post "/signup" (SignUpHandler.signUpHandler dbConn)

        post "/signin" (SignInHandler.signInHandler secret sessionTime dbConn)

        post "/loggedHandler" (LoggedHandler.loggedHandler secret sessionTime dbConn)

        get "/products/:productId" (ProductsHandler.getProduct secret sessionTime bucket dbConn)

        post "/products/calculatePrice" (ProductsHandler.calculatePrice secret sessionTime bucket dbConn)

        get "/products" (ProductsHandler.listProducts secret sessionTime bucket dbConn)

        get "/orders/:orderId" (OrdersHandler.getOrderById secret sessionTime bucket dbConn)

        get "/orders" (OrdersHandler.listOrdersByUser secret sessionTime bucket dbConn)

        post "/orders" (OrdersHandler.createOrder secret sessionTime bucket dbConn)

        post "/orders/interactions" (OrdersHandler.createOrderInteraction secret sessionTime dbConn)

        post "/images" (ImagesHandler.putImages secret sessionTime bucket dbConn)

        post "/recoveryPassword" (AccountRecoveryHandler.requestRecoveryCode sessionTime dbConn)

        post "/resetPassword" (AccountRecoveryHandler.resetPassword dbConn)
      