{-# LANGUAGE OverloadedStrings #-}

module AccountRecoveryHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Database.MySQL.Simple
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Control.Monad.IO.Class
import Network.Wai.Test
import Network.HTTP.Types
import App
import Control.Monad
import Data.Maybe
import qualified Text.Regex.Pcre2 as Regex
import qualified Data.Aeson as Aeson
import qualified Security.Jwt as Jwt

cleanDb :: Connection -> IO ()
cleanDb dbConn =
  execute dbConn "TRUNCATE TABLE users" () >>
  execute dbConn "TRUNCATE TABLE permissions" () >>
  execute dbConn "TRUNCATE TABLE users_permissions" () >>
  execute dbConn "INSERT INTO permissions (permission) VALUES ('userLevel');" () >>
  execute dbConn "INSERT INTO permissions (permission) VALUES ('adminLevel')" () >>
  execute dbConn "TRUNCATE TABLE recovery_code" () >>
  return ()

createUser :: Connection -> WaiSession st ()
createUser dbConn = do
  post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
  void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))

createUserRequest :: DBL.ByteString
createUserRequest = "{ \
\    \"name\": \"Andre\", \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

userNotFoundRequest :: DBL.ByteString
userNotFoundRequest = "{\"email\": \"email@example.com.br\"}"

sendCodeRequest :: BSLU.ByteString
sendCodeRequest = "{\"email\":\"email@example.com\"}"

incorrectResetPasswordRequest = "{ \
\    \"email\": \"email@example.com\", \
\    \"code\": \"123456\", \
\    \"newPassword\": \"anotherPassword\" \
\}"

resetPasswordRequest = "{ \
\    \"email\": \"email@example.com\", \
\    \"code\": \"306013\", \
\    \"newPassword\": \"anotherPassword\" \
\}"


suiteSpec :: Connection -> String -> String -> String -> String -> String -> Spec
suiteSpec dbConn host database user password bucket = do

  with (api host database user password "secret2" 60 bucket) $ do
    describe "AccountRecoveryHandlerSpec" $ do

      it "Should not send token if user not found" $ do
        liftIO (cleanDb dbConn)
        _ <- createUser dbConn
        post "/recoveryPassword" userNotFoundRequest `shouldRespondWith` "{\"code\":3,\"message\":\"User not found\"}" { matchStatus = 404 }

      it "Should send token if user is ok" $ do
        liftIO (cleanDb dbConn)
        _ <- createUser dbConn
        post "/recoveryPassword" sendCodeRequest `shouldRespondWith` "success" { matchStatus = 200 }

      it "Should not recovery password with bad code" $ do
        liftIO (cleanDb dbConn)
        _ <- createUser dbConn
        [Only oldPasswordHash] <- liftIO (query dbConn "SELECT hashed_password FROM users WHERE id = ?" (Only 1 :: Only Int) :: (IO [Only String]))
        void (liftIO (execute dbConn "INSERT INTO recovery_code (user_id, hashed_code, expiration, status) VALUES (?, ?, ?, ?)" (1 :: Int, "$2b$10$bd19AWNXg7TBHYZ/UasPnuI4DfAlhz43dXoq5EedNCsCSoF6E.o8e" :: String, 99999999999 :: Int, "active" :: String)))
        post "/resetPassword" incorrectResetPasswordRequest `shouldRespondWith` "{\"code\":6,\"message\":\"Incorrect recovery code\"}" { matchStatus = 400 }
        [Only newPasswordHash] <- liftIO (query dbConn "SELECT hashed_password FROM users WHERE id = ?" (Only 1 :: Only Int))
        liftIO (newPasswordHash `shouldBe` oldPasswordHash)
        return ()  

      it "Should recovery password" $ do
        liftIO (cleanDb dbConn)
        _ <- createUser dbConn
        [Only oldPasswordHash] <- liftIO (query dbConn "SELECT hashed_password FROM users WHERE id = ?" (Only 1 :: Only Int) :: (IO [Only String]))
        void (liftIO (execute dbConn "INSERT INTO recovery_code (user_id, hashed_code, expiration, status) VALUES (?, ?, ?, ?)" (1 :: Int, "$2b$10$bd19AWNXg7TBHYZ/UasPnuI4DfAlhz43dXoq5EedNCsCSoF6E.o8e" :: String, 99999999999 :: Int, "active" :: String)))
        post "/resetPassword" resetPasswordRequest `shouldRespondWith` "success" { matchStatus = 200 }
        [Only newPasswordHash] <- liftIO (query dbConn "SELECT hashed_password FROM users WHERE id = ?" (Only 1 :: Only Int))
        liftIO (newPasswordHash `shouldNotBe` oldPasswordHash)
        return ()
