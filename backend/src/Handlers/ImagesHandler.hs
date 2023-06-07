module Handlers.ImagesHandler
    ( putImages
    ) where

import Web.Scotty
import Web.Scotty.Internal.Types
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class

import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Model.Image as Image
import qualified Transport.OrderCreatedResponse as OrderCreatedResponse

import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified System.IO as IO

import Errors ( invalidSessionError )
import qualified Data.UUID.V4 as UUID

unauthorizedResponse :: ActionT TL.Text IO ()
unauthorizedResponse = status unauthorized401 >> json invalidSessionError


putImages secret sessionTime conn = HandlersCommons.handleLoggedFilesRequest secret sessionTime conn "userLevel" unauthorizedResponse (\files _ -> do
                imagesIds <- liftIO (mapM_ (\(contents, _) -> uploadFile conn contents) files)
                json imagesIds
                )

uploadFile conn fileContents = do
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

    -- The payload (and hash) for the S3 object is retrieved from a FilePath,
    -- either hashedFile or chunkedFile can be used, with the latter ensuring
    -- the contents of the file is enumerated exactly once, during send:
    body <- AWS.chunkedFile AWS.defaultChunkSize "/home/andre/requirements.txt"

    key <- UUID.nextRandom


    -- We now run the AWS computation with the overriden logger, performing the
    -- PutObject request. $sel:_envRegion:Env or within can be used to set the
    -- remote AWS Region:
    response <- AWS.runResourceT $
        AWS.send env (S3.newPutObject "tiozao-avm" key fileContents)

    --let url = response

    Image.saveImage conn url

    