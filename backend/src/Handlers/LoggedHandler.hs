module Handlers.LoggedHandler
    ( loggedHandler
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
import Errors ( invalidJsonError, invalidSessionError )

invalidJsonResponse = status badRequest400 >> json invalidJsonError
unauthorizedResponse = status unauthorized401 >> json invalidSessionError

loggedHandler secret sessionTime conn = HandlersCommons.handleLoggedJsonRequest secret sessionTime conn "userLevel" invalidJsonResponse unauthorizedResponse (\user session -> do
                hashedPassword <- Password.hashPassword (CreateUserRequest.password user)
                let userWithHashedPassword = user { CreateUserRequest.password = TL.unpack hashedPassword }
                userId <- liftIO (User.saveUser conn userWithHashedPassword)
                case userId of
                    Nothing -> text (TL.pack (show session))
                    Just uId -> text (TL.pack (show session))
                )