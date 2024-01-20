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
import qualified Amazonka.S3 as S3

import Errors ( invalidSessionError )

unauthorizedResponse = status unauthorized401 >> json invalidSessionError

putImages secret sessionTime bucket conn = HandlersCommons.handleLoggedFilesRequest secret sessionTime conn "userLevel" unauthorizedResponse (\files _ -> do
                imagesKeys <- liftIO (mapM (AwsUtils.uploadFile (S3.BucketName (T.pack bucket))) files)
                images <- liftIO (mapM (Image.saveImage conn bucket) (map T.unpack imagesKeys))
                let imageIds = map Image.imageId images
                json imageIds
                )

