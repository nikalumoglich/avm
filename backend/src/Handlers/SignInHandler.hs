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
import qualified Model.User as User
import qualified Transport.SignInRequest as SignInRequest
import qualified Handlers.HandlerCommons as HandlersCommons

signInHandler :: Connection -> ActionT TL.Text IO ()
signInHandler conn = HandlersCommons.handleJsonRequest (text "Invalid JSON") (\signInRequest -> do
                user <- liftIO (User.getUserByEmail conn (SignInRequest.email signInRequest))
                case user of
                    Left error' -> text $ TL.pack error'
                    Right user' ->
                        if Password.comparePassword (User.password user') (SignInRequest.password signInRequest)
                            then do
                                text "OK"
                            else do
                                text "Not OK"
                )