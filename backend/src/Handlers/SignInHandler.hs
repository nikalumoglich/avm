module Handlers.SignInHandler
    ( signInHandler
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import Database.MySQL.Simple
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import qualified Security.Password as Password
import qualified Security.Jwt as Jwt
import qualified Model.User as User
import qualified Model.Session as Session
import qualified Transport.SignInRequest as SignInRequest
import qualified Handlers.HandlerCommons as HandlersCommons
import Errors

signInHandler secret sessionTime conn = HandlersCommons.handleJsonRequest (status badRequest400 >> json invalidJsonError) (\signInRequest -> do
                user <- liftIO (User.getUserByEmail conn (SignInRequest.email signInRequest))
                case user of
                    User.UserNotFound -> status notFound404 >> json userNotFoundError
                    user' ->
                        if Password.comparePassword (User.password user') (SignInRequest.password signInRequest)
                            then do
                                session <- liftIO (Session.saveSession conn sessionTime user')
                                encodedToken <- liftIO (Jwt.encodeSession secret session)
                                json encodedToken
                            else do
                                status unauthorized401
                                json wrongPasswordError
                )