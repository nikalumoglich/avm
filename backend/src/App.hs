{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App
    ( app
    ) where

import Web.Scotty
import qualified Data.Aeson as Aeson
import Database.MySQL.Simple
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import qualified Security.Password as Password
import qualified Model.User as User
import qualified Transport.CreateUserRequest as CreateUserRequest
import qualified Transport.SignInRequest as SignInRequest

app :: IO ()
app = do
    conn <- connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = "avm" })

    scotty 3000 $ do
        post "/signup" $ do
            requestBody <- body
            let maybeUser = Aeson.decode requestBody :: Maybe CreateUserRequest.CreateUserRequest
            case maybeUser of
                Nothing -> text "Invalid JSON"
                Just user -> do
                    hashedPassword <- Password.hashPassword (CreateUserRequest.password user)
                    let userWithHashedPassword = user { CreateUserRequest.password = TL.unpack hashedPassword }
                    userId <- liftIO (User.saveUser conn userWithHashedPassword)
                    text (TL.pack ("User created, id: " ++ show userId))

        post "/signin" $ do
            requestBody <- body
            let maybeSignInRequest = Aeson.decode requestBody :: Maybe SignInRequest.SignInRequest
            case maybeSignInRequest of
                Nothing -> text "Invalid JSON"
                Just signInRequest -> do
                    user <- liftIO (User.getUserByEmail conn (SignInRequest.email signInRequest))
                    case user of
                        Left error' -> text $ TL.pack error'
                        Right user' ->
                            if Password.comparePassword (User.password user') (SignInRequest.password signInRequest)
                                then text "OK"
                                else text "Not OK"


