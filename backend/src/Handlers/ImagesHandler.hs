{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.ImagesHandler
    ( putImages
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class

import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Model.Image as Image
import qualified Transport.OrderCreatedResponse as OrderCreatedResponse

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified System.IO as IO

import Errors ( invalidSessionError )
import qualified Data.UUID.V4 as V4
import qualified Data.UUID as UUID
import qualified Network.Wai.Parse
import qualified Data.ByteString.Lazy.UTF8 as BLU 
import qualified Data.ByteString.UTF8 as BSU 

import qualified Data.Time

unauthorizedResponse :: ActionT TL.Text IO ()
unauthorizedResponse = status unauthorized401 >> json invalidSessionError


putImages secret sessionTime conn = HandlersCommons.handleLoggedFilesRequest secret sessionTime conn "userLevel" unauthorizedResponse (\files _ -> do
                liftIO (putStrLn "Handler fileContents")
                liftIO (putStrLn (TL.unpack (fst (head files))))
                imagesIds <- liftIO (mapM (uploadFile conn) files)
                let iids = map (\x -> BSU.toString x) imagesIds
                let x = mconcat iids
                text (TL.pack x)--(TL.pack (show (length files)))
                )

uploadFile :: p -> (a, Network.Wai.Parse.FileInfo BLU.ByteString) -> IO AWS.ByteString
uploadFile conn (_, fileInfo) = do
    -- A new Logger to replace the default noop logger is created, with the logger
    -- set to print debug information and errors to stdout:
    logger <- AWS.newLogger AWS.Debug IO.stdout

    -- To specify configuration preferences, newEnv is used to create a new
    -- configuration environment. The Credentials parameter is used to specify
    -- mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case Discover will cause the library to try a number of options such
    -- as default environment variables, or an instance's IAM Profile and identity document:
    discover <- AWS.newEnv AWS.Discover

    let env =
            discover
                { AWS._envLogger = logger
                , AWS._envRegion = AWS.SaoPaulo
                }

    key <- V4.nextRandom
    let s3Key = (S3.ObjectKey (UUID.toText key))

    let fileContents = BLU.toString (Network.Wai.Parse.fileContent fileInfo)

    -- We now run the AWS computation with the overriden logger, performing the
    -- PutObject request. $sel:_envRegion:Env or within can be used to set the
    -- remote AWS Region:
    AWS.runResourceT $
        AWS.send env (S3.newPutObject "tiozao-avm" s3Key (AWS.Hashed (AWS.toHashed fileContents)))

    getPresignedURL AWS.SaoPaulo "tiozao-avm" s3Key

getPresignedURL :: p -> S3.BucketName -> S3.ObjectKey -> IO AWS.ByteString
getPresignedURL reg b k = do
    logger <- AWS.newLogger AWS.Debug IO.stdout
    discover <- AWS.newEnv AWS.Discover

    let env =
            discover
                { AWS._envLogger = logger
                , AWS._envRegion = AWS.SaoPaulo
                }

    ts <- Data.Time.getCurrentTime
    AWS.runResourceT $ AWS.presignURL env ts 60 (S3.newGetObject b k)


    --let url = response

    -- Image.saveImage conn url

