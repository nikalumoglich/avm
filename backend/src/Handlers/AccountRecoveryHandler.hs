module Handlers.AccountRecoveryHandler
    ( requestRecoveryCode
    ) where

import Web.Scotty
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as TL
import qualified System.Random as Random
import Data.Time.Clock.POSIX
import qualified Security.Password as Password
import qualified Model.User as User
import qualified Model.RecoveryCode as RecoveryCode
import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Transport.AccessCodeRequest as AccessCodeRequest
import Errors ( invalidJsonError, userNotFoundError )

import qualified Controller.AwsUtils as AwsUtils

invalidJsonResponse = status badRequest400 >> json invalidJsonError
userNotFoundResponse = status notFound404 >> json userNotFoundError

getListOfRandomDigits n = do
    digitsStrings <- Control.Monad.replicateM n (Random.randomRIO (0,9 :: Int))
    return (foldl' (++) "" (map show digitsStrings))

requestRecoveryCode sessionTime conn = HandlersCommons.handleJsonRequest invalidJsonResponse (\request -> do
    let email = AccessCodeRequest.email request
    user' <- liftIO (User.getUserByEmail conn email)

    case user' of
        User.UserNotFound -> userNotFoundResponse
        user -> do
            plainTextRecoveryCode <- liftIO $ getListOfRandomDigits 6
            hashedRecoveryCode <- Password.hashPassword plainTextRecoveryCode
            currentTimestamp <- liftIO getPOSIXTime
            let expirationTime = round (currentTimestamp + realToFrac sessionTime) :: Int
            _ <- liftIO $ RecoveryCode.saveRecoveryCode conn user (TL.unpack hashedRecoveryCode) expirationTime
            _ <- liftIO $ AwsUtils.sendEmail (User.email user) "AVM Recovery Code" plainTextRecoveryCode
            text $ TL.pack "success"

    )
