{-# LANGUAGE OverloadedStrings #-}

module Handlers.SignUpHandler
    ( signUpHandler
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Database.MySQL.Simple
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import qualified Security.Password as Password
import qualified Model.User as User
import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Transport.CreateUserRequest as CreateUserRequest


signUpHandler :: Connection -> ActionT TL.Text IO ()
signUpHandler conn = HandlersCommons.handleJsonRequest (text "Invalid JSON") (\user -> do
                hashedPassword <- Password.hashPassword (CreateUserRequest.password user)
                let userWithHashedPassword = user { CreateUserRequest.password = TL.unpack hashedPassword }
                userId <- liftIO (User.saveUser conn userWithHashedPassword)
                text (TL.pack ("User created, id: " ++ show userId))
                )