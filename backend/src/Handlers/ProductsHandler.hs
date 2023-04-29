module Handlers.ProductsHandler
    ( listProducts
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class
import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Model.Product as Product
import Errors ( invalidSessionError )


unauthorizedResponse :: ActionT TL.Text IO ()
unauthorizedResponse = status unauthorized401 >> json invalidSessionError

listProducts secret sessionTime conn = HandlersCommons.handleLoggedRequest secret sessionTime conn "userLevel" unauthorizedResponse (\_ -> do
                products <- liftIO (Product.listProducts conn)
                json products
                )