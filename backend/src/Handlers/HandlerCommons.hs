{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Handlers.HandlerCommons
    ( handleJsonRequest
    , handleLoggedJsonRequest
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as TL

import Database.MySQL.Simple
import Control.Monad.IO.Class
import qualified Security.Jwt as Jwt
import Model.Session

handleJsonRequest :: Aeson.FromJSON t => ActionT TL.Text IO b -> (t -> ActionT TL.Text IO b) -> ActionT TL.Text IO b
handleJsonRequest errorHandler successHandler = do
    requestBody <- body
    let maybeJson = Aeson.decode requestBody
    case maybeJson of
        Nothing -> errorHandler
        Just json' -> successHandler json'

handleLoggedJsonRequest :: Aeson.FromJSON t => String -> Connection -> ActionT TL.Text IO b -> ActionT TL.Text IO b -> (t -> Session -> ActionT TL.Text IO b) -> ActionT TL.Text IO b 
handleLoggedJsonRequest secret conn invalidJsonErrorHandler invalidTokenErrorHandler successHandler = do
    authorizationHeader <- header $ TL.pack "Authorization"
    case authorizationHeader of
        Nothing -> invalidTokenErrorHandler
        Just headerContents -> do
            let token = TL.replace "Bearer " "" headerContents
            let tokenSession = Jwt.decodeSession secret (TL.unpack token)
            session <- liftIO (getActiveSession conn (userId tokenSession))
            case session of
                SessionNotFound -> invalidTokenErrorHandler
                session' -> do
                    requestBody <- body
                    let maybeJson = Aeson.decode requestBody
                    case maybeJson of
                        Nothing -> invalidJsonErrorHandler
                        Just json' -> successHandler json' session'