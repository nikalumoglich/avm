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
import System.IO.Error
import qualified Handlers.SignUpHandler as SignUpHandler
import qualified Handlers.SignInHandler as SignInHandler
import qualified Handlers.LoggedHandler as LoggedHandler
import qualified Handlers.ProductsHandler as ProductsHandler

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
    version <- getEnvOrDefault "AVM_VERSION" "version not defined"

    putStrLn ("AVM running on port " ++ port ++ " - version: " ++ version)

    case shouldStart of
        True -> api host database user password secret >>= run (read port)
        False -> return ()    

api :: String -> String -> String -> String -> String -> IO Application
api host database user password secret = do
    dbConn <- connect (defaultConnectInfo { connectHost = host, connectUser = user, connectPassword = password, connectDatabase = database })
    
    scottyApp $ do

        post "/signup" (SignUpHandler.signUpHandler dbConn)

        post "/signin" (SignInHandler.signInHandler secret dbConn)

        post "/loggedHandler" (LoggedHandler.loggedHandler secret dbConn)

        get "/products" (ProductsHandler.listProducts secret dbConn)
