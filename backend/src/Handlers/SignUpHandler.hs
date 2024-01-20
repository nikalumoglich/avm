module Handlers.SignUpHandler
    ( signUpHandler
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import Database.MySQL.Simple
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import qualified Security.Password as Password
import qualified Model.User as User
import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Transport.CreateUserRequest as CreateUserRequest
import qualified Transport.UserCreatedResponse as UserCreatedResponse
import Errors


signUpHandler conn = HandlersCommons.handleJsonRequest (status badRequest400 >> json invalidJsonError) (\user -> do
                hashedPassword <- Password.hashPassword (CreateUserRequest.password user)
                let userWithHashedPassword = user { CreateUserRequest.password = TL.unpack hashedPassword }
                userId <- liftIO (User.saveUser conn userWithHashedPassword)
                case userId of
                    Nothing -> status badRequest400 >> json userAlreadyExistError
                    Just uId -> json UserCreatedResponse.UserCreatedResponse { UserCreatedResponse.userId = uId }
                )