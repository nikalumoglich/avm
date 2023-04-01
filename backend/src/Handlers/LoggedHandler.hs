{-# LANGUAGE OverloadedStrings #-}

module Handlers.LoggedHandler
    ( loggedHandler
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


loggedHandler :: String -> Connection -> ActionT TL.Text IO ()
loggedHandler secret conn = HandlersCommons.handleLoggedJsonRequest secret conn "userLevel" (text "Invalid JSON") (text "Invalid Token") (\user session -> do
                hashedPassword <- Password.hashPassword (CreateUserRequest.password user)
                let userWithHashedPassword = user { CreateUserRequest.password = TL.unpack hashedPassword }
                userId <- liftIO (User.saveUser conn userWithHashedPassword)
                case userId of
                    Nothing -> text (TL.pack (show session))
                    Just uId -> text (TL.pack (show session))
                )