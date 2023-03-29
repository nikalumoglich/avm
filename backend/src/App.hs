{-# LANGUAGE OverloadedStrings #-}

module App
    ( app
    , api
    ) where

import Web.Scotty
import Database.MySQL.Simple
import qualified Handlers.SignUpHandler as SignUpHandler
import qualified Handlers.SignInHandler as SignInHandler
import qualified Handlers.LoggedHandler as LoggedHandler
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

app :: IO ()
app = do
    -- get database params from env
    api "avm" >>= run 3000

api :: String -> IO Application
api database = do
    dbConn <- connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = database })
    let secret = "secret"

    scottyApp $ do

        post "/signup" (SignUpHandler.signUpHandler dbConn)

        post "/signin" (SignInHandler.signInHandler dbConn)

        post "/loggedHandler" (LoggedHandler.loggedHandler secret dbConn)
