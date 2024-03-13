module Handlers.AccountRecoveryHandler
    ( requestRecoveryCode
    , resetPassword
    ) where

import Web.Scotty
import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import qualified Data.Text.Lazy as TL
import qualified System.Random as Random
import Data.Time.Clock.POSIX
import qualified Security.Password as Password
import qualified Model.User as User
import qualified Model.RecoveryCode as RecoveryCode
import qualified Handlers.HandlerCommons as HandlersCommons
import qualified Transport.AccessCodeRequest as AccessCodeRequest
import qualified Transport.ResetPasswordRequest as ResetPasswordRequest
import Errors ( invalidJsonError, userNotFoundError, incorrectRecoveryCode )

import qualified Controller.AwsUtils as AwsUtils

invalidJsonResponse = status badRequest400 >> json invalidJsonError
userNotFoundResponse = status notFound404 >> json userNotFoundError
incorrectRecoveryCodeResponse = status badRequest400 >> json incorrectRecoveryCode

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

resetPassword conn = HandlersCommons.handleJsonRequest invalidJsonResponse (\request -> do
    let email = ResetPasswordRequest.email request
    user' <- liftIO (User.getUserByEmail conn email)

    case user' of
        User.UserNotFound -> userNotFoundResponse
        user -> do
            activeRecoveryCode' <- liftIO $ RecoveryCode.getActiveRecoveryCode conn (User.userId user)
            case activeRecoveryCode' of 
                RecoveryCode.RecoveryCodeNotFound -> incorrectRecoveryCodeResponse
                activeRecoveryCode -> do
                    if Password.comparePassword (RecoveryCode.hashedCode activeRecoveryCode) (ResetPasswordRequest.code request) then do
                        _ <- liftIO $ RecoveryCode.inactivateRecoveryCode conn (RecoveryCode.recoveryCodeId activeRecoveryCode)
                        hashedPassword <- Password.hashPassword (ResetPasswordRequest.newPassword request)
                        _ <- liftIO $ User.changePassword conn (User.userId user) hashedPassword
                        text $ TL.pack "success"
                    else incorrectRecoveryCodeResponse

    )
