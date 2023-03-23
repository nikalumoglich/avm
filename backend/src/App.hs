{-# LANGUAGE OverloadedStrings #-}

module App
    ( app
    ) where

import Web.Scotty
import Database.MySQL.Simple
import qualified Handlers.SignUpHandler as SignUpHandler
import qualified Handlers.SignInHandler as SignInHandler

app :: IO ()
app = do
    dbConn <- connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = "avm" })

    scotty 3000 $ do
        post "/signup" (SignUpHandler.signUpHandler dbConn)

        post "/signin" (SignInHandler.signInHandler dbConn)
