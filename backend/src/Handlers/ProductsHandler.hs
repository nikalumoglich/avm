module Handlers.ProductsHandler
    ( listProducts
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import Database.MySQL.Simple
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class
import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Model.Product as Product
import Errors ( invalidJsonError, invalidSessionError )


invalidJsonResponse :: ActionT TL.Text IO ()
invalidJsonResponse = status badRequest400 >> json invalidJsonError
unauthorizedResponse :: ActionT TL.Text IO ()
unauthorizedResponse = status unauthorized401 >> json invalidSessionError

listProducts :: String -> Connection -> ActionT TL.Text IO ()
listProducts secret conn = HandlersCommons.handleLoggedRequest secret conn "userLevel" invalidJsonResponse unauthorizedResponse (\_ -> do
                products <- liftIO (Product.listProducts conn)
                json products
                )