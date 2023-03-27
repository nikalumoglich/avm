{-# LANGUAGE OverloadedStrings #-}

module Handlers.SignInHandler
    ( signInHandler
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Database.MySQL.Simple
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import qualified Security.Password as Password
import qualified Security.Jwt as Jwt
import qualified Model.User as User
import qualified Model.Session as Session
import qualified Transport.SignInRequest as SignInRequest
import qualified Handlers.HandlerCommons as HandlersCommons

signInHandler :: Connection -> ActionT TL.Text IO ()
signInHandler conn = HandlersCommons.handleJsonRequest (text "Invalid JSON") (\signInRequest -> do
                user <- liftIO (User.getUserByEmail conn (SignInRequest.email signInRequest))
                case user of
                    User.Error errorMessage -> text $ TL.pack errorMessage
                    User.UserNotFound -> text $ TL.pack "User not found"
                    user' ->
                        if Password.comparePassword (User.password user') (SignInRequest.password signInRequest)
                            then do
                                session <- liftIO (Session.saveSession conn user')
                                encodedToken <- liftIO (Jwt.encodeSession "secret" session)
                                text (TL.pack encodedToken)
                            else do
                                text "Not OK"
                )