{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.ImagesHandler
    ( putImages
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class

import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Model.Image as Image
import qualified Controller.AwsUtils as AwsUtils

import Errors ( invalidSessionError )
unauthorizedResponse :: ActionT TL.Text IO ()
unauthorizedResponse = status unauthorized401 >> json invalidSessionError

putImages secret sessionTime conn = HandlersCommons.handleLoggedFilesRequest secret sessionTime conn "userLevel" unauthorizedResponse (\files _ -> do
                imagesKeys <- liftIO (mapM (AwsUtils.uploadFile "tiozao-avm") files)
                images <- liftIO (mapM (Image.saveImage conn) (map T.unpack imagesKeys))
                let imageIds = map Image.imageId images
                json imageIds
                )

