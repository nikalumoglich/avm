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

import qualified Network.Wai.Parse
import qualified Data.ByteString.Lazy.UTF8 as BSU

import Database.MySQL.Simple
import Control.Monad.IO.Class
import qualified Security.Jwt as Jwt
import qualified Model.User as User
import qualified Model.Session as Session
import qualified Model.Permission as Permission

handleJsonRequest :: Aeson.FromJSON t => ActionT TL.Text IO b -> (t -> ActionT TL.Text IO b) -> ActionT TL.Text IO b
handleJsonRequest errorHandler successHandler = do
    requestBody <- body
    let maybeJson = Aeson.decode requestBody
    case maybeJson of
        Nothing -> errorHandler
        Just json' -> successHandler json'

handleLoggedJsonRequest :: Aeson.FromJSON t => String -> Int -> Connection -> String -> ActionT TL.Text IO b -> ActionT TL.Text IO b -> (t -> Session.Session -> ActionT TL.Text IO b) -> ActionT TL.Text IO b
handleLoggedJsonRequest secret sessionTime conn requiredPermission invalidJsonErrorHandler invalidTokenErrorHandler successHandler = do
    authorizationHeader <- header $ TL.pack "Authorization"
    case authorizationHeader of
        Nothing -> invalidTokenErrorHandler
        Just headerContents -> do
            let token = TL.replace "Bearer " "" headerContents
            let tokenSession = Jwt.decodeSession secret (TL.unpack token)
            case tokenSession of
                Session.SessionNotFound -> invalidTokenErrorHandler
                sessionFromToken -> do
                    session <- liftIO (Session.getActiveSession conn (Session.userId sessionFromToken))
                    case session of
                        Session.SessionNotFound -> invalidTokenErrorHandler
                        session' -> do
                            user <- liftIO (User.getUserById conn (Session.userId session'))
                            permissions <- liftIO (Permission.getUserPermissions conn (User.userId user))
                            (if any (\permission -> Permission.permission permission == requiredPermission) permissions then (do
                                requestBody <- body
                                let maybeJson = Aeson.decode requestBody
                                case maybeJson of
                                    Nothing -> invalidJsonErrorHandler
                                    Just json' -> liftIO (Session.renewSession conn sessionTime session') >> successHandler json' session') else invalidTokenErrorHandler)

handleLoggedRequest :: String -> Int -> Connection -> String -> ActionT TL.Text IO b -> (Session.Session -> ActionT TL.Text IO b) -> ActionT TL.Text IO b
handleLoggedRequest secret sessionTime conn requiredPermission invalidTokenErrorHandler successHandler = do
    authorizationHeader <- header $ TL.pack "Authorization"
    case authorizationHeader of
        Nothing -> invalidTokenErrorHandler
        Just headerContents -> do
            let token = TL.replace "Bearer " "" headerContents
            let tokenSession = Jwt.decodeSession secret (TL.unpack token)
            case tokenSession of
                Session.SessionNotFound -> invalidTokenErrorHandler
                sessionFromToken -> do
                    session <- liftIO (Session.getActiveSession conn (Session.userId sessionFromToken))
                    case session of
                        Session.SessionNotFound -> invalidTokenErrorHandler
                        session' -> do
                            user <- liftIO (User.getUserById conn (Session.userId session'))
                            permissions <- liftIO (Permission.getUserPermissions conn (User.userId user))
                            (if any (\permission -> Permission.permission permission == requiredPermission) permissions then 
                                liftIO (Session.renewSession conn sessionTime session') >> successHandler session' else invalidTokenErrorHandler)

handleLoggedFilesRequest :: String -> Int -> Connection -> String -> ActionT TL.Text IO b -> ([File] -> Session.Session -> ActionT TL.Text IO b) -> ActionT TL.Text IO b 
handleLoggedFilesRequest secret sessionTime conn requiredPermission invalidTokenErrorHandler successHandler = do
    authorizationHeader <- header $ TL.pack "Authorization"
    case authorizationHeader of
        Nothing -> invalidTokenErrorHandler
        Just headerContents -> do
            let token = TL.replace "Bearer " "" headerContents
            let tokenSession = Jwt.decodeSession secret (TL.unpack token)
            case tokenSession of
                Session.SessionNotFound -> invalidTokenErrorHandler
                sessionFromToken -> do
                    session <- liftIO (Session.getActiveSession conn (Session.userId sessionFromToken))
                    case session of
                        Session.SessionNotFound -> invalidTokenErrorHandler
                        session' -> do
                            user <- liftIO (User.getUserById conn (Session.userId session'))
                            permissions <- liftIO (Permission.getUserPermissions conn (User.userId user))
                            (if any (\permission -> Permission.permission permission == requiredPermission) permissions then (do
                                requestFiles <- files
                                liftIO (Session.renewSession conn sessionTime session') >> successHandler requestFiles session') else invalidTokenErrorHandler)
