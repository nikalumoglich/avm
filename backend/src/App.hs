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

getEnvOrDefault :: String -> String -> IO String
getEnvOrDefault name defaultValue = getEnv name `catch` handleIsDoesNotExistError defaultValue

handleIsDoesNotExistError :: String -> System.IO.Error.IOError -> IO String
handleIsDoesNotExistError defaultValue _ = return defaultValue

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
    version <- getEnvOrDefault "AVM_VERSION" "version not defined"

    putStrLn ("AVM running on port " ++ port ++ " - version: " ++ version)

    when shouldStart $ api host database user password secret sessionTime >>= run (read port)

api :: String -> String -> String -> String -> String -> Int -> IO Application
api host database user password secret sessionTime = do
    dbConn <- connect (defaultConnectInfo { connectHost = host, connectUser = user, connectPassword = password, connectDatabase = database })

    scottyApp $ do

        post "/signup" (SignUpHandler.signUpHandler dbConn)

        post "/signin" (SignInHandler.signInHandler secret sessionTime dbConn)

        post "/loggedHandler" (LoggedHandler.loggedHandler secret sessionTime dbConn)

        get "/products/:productId" (ProductsHandler.getProduct secret sessionTime dbConn)

        post "/products/calculatePrice" (ProductsHandler.calculatePrice secret sessionTime dbConn)

        get "/products" (ProductsHandler.listProducts secret sessionTime dbConn)

        get "/orders" (OrdersHandler.listOrdersByUser secret sessionTime dbConn)

        post "/orders" (OrdersHandler.createOrder secret sessionTime dbConn)
        
