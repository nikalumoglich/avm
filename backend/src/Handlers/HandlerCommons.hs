{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Handlers.HandlerCommons
    ( handleJsonRequest
    , handleLoggedJsonRequest
    , handleLoggedRequest
    , handleLoggedFilesRequest
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as TL

import Database.MySQL.Simple
import Control.Monad.IO.Class
import qualified Security.Jwt as Jwt
import qualified Model.User as User
import qualified Model.Session as Session
import qualified Model.Permission as Permission

handleJsonRequest errorHandler successHandler = do
    requestBody <- body
    let maybeJson = Aeson.decode requestBody
    case maybeJson of
        Nothing -> errorHandler
        Just json' -> successHandler json'

handleLoggedJsonRequest secret sessionTime conn requiredPermission invalidJsonErrorHandler invalidTokenErrorHandler successHandler = do
    session <- getSession secret sessionTime conn requiredPermission
    case session of
        Session.SessionNotFound -> invalidTokenErrorHandler
        session' -> do
            requestBody <- body
            let maybeJson = Aeson.decode requestBody
            case maybeJson of
                Nothing -> invalidJsonErrorHandler
                Just json' -> successHandler json' session'

handleLoggedRequest secret sessionTime conn requiredPermission invalidTokenErrorHandler successHandler = do
    session <- getSession secret sessionTime conn requiredPermission
    case session of
        Session.SessionNotFound -> invalidTokenErrorHandler
        session' -> successHandler session'
    

handleLoggedFilesRequest secret sessionTime conn requiredPermission invalidTokenErrorHandler successHandler = do
    session <- getSession secret sessionTime conn requiredPermission
    case session of
        Session.SessionNotFound -> invalidTokenErrorHandler
        session' -> do
            requestFiles <- files
            successHandler requestFiles session'

getSession secret sessionTime conn requiredPermission = do
    authorizationHeader <- header $ TL.pack "Authorization"
    case authorizationHeader of
        Nothing -> return Session.SessionNotFound
        Just headerContents -> do
            let token = TL.replace "Bearer " "" headerContents
            let tokenSession = Jwt.decodeSession secret (TL.unpack token)
            case tokenSession of
                Session.SessionNotFound -> return Session.SessionNotFound
                sessionFromToken -> do
                    session <- liftIO (Session.getActiveSession conn (Session.userId sessionFromToken))
                    case session of
                        Session.SessionNotFound -> return Session.SessionNotFound
                        session' -> do
                            user <- liftIO (User.getUserById conn (Session.userId session'))
                            permissions <- liftIO (Permission.getUserPermissions conn (User.userId user))
                            (if any (\permission -> Permission.permission permission == requiredPermission) permissions then 
                                liftIO (Session.renewSession conn sessionTime session') >> return session' else return Session.SessionNotFound)