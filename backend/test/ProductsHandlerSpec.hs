{-# LANGUAGE OverloadedStrings #-}

module ProductsHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Database.MySQL.Simple
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
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
import qualified Data.Aeson as Aeson
import qualified Security.Jwt as Jwt

shouldRespondWithPredicate :: Control.Monad.IO.Class.MonadIO m => m SResponse -> (TL.Text -> Bool, String) -> m ()
shouldRespondWithPredicate action (matcher, errorMessage) = do
  r <- action
  if matcher (EL.decodeUtf8  (simpleBody r))
    then mapM_ (liftIO . expectationFailure) []
    else mapM_ (liftIO . expectationFailure) [errorMessage]

cleanDb :: Connection -> IO ()
cleanDb dbConn =
  execute dbConn "TRUNCATE TABLE users" () >> 
  execute dbConn "TRUNCATE TABLE permissions" () >>
  execute dbConn "TRUNCATE TABLE users_permissions" () >>
  execute dbConn "INSERT INTO permissions (permission) VALUES ('userLevel');" () >>
  execute dbConn "INSERT INTO permissions (permission) VALUES ('adminLevel')" () >>
  execute dbConn "TRUNCATE TABLE products" () >>
  execute dbConn "TRUNCATE TABLE images" () >>
  execute dbConn "TRUNCATE TABLE products_images" () >>
  execute dbConn "TRUNCATE TABLE dimensions" () >>
  execute dbConn "TRUNCATE TABLE dimensions_images" () >>
  return ()

createProduct :: Connection -> IO ()
createProduct dbConn = 
  execute dbConn "INSERT INTO products (name, description, price_formula) VALUES ('table', 'just a table', '{height} * {width} * {depth}')" () >>
  execute dbConn "INSERT INTO images (url) VALUES ('https://tiozao.co/image1.png')" () >>
  execute dbConn "INSERT INTO products_images (product_id, image_id) VALUES (1, 1)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'height', '{height}')" () >>
  execute dbConn "INSERT INTO images (url) VALUES ('https://tiozao.co/image2.png')" () >>
  execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 2)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'width', '{width}')" () >>
  execute dbConn "INSERT INTO images (url) VALUES ('https://tiozao.co/image3.png')" () >>
  execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 3)" () >>
  execute dbConn "INSERT INTO dimensions (product_id, name, symbol) VALUES (1, 'depth', '{depth}')" () >>
  execute dbConn "INSERT INTO images (url) VALUES ('https://tiozao.co/image4.png')" () >>
  execute dbConn "INSERT INTO dimensions_images (dimension_id, image_id) VALUES (1, 4)" () >>
  return ()

createUser :: Aeson.FromJSON b => Connection -> WaiSession st b
createUser dbConn = do
  post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
  void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
  response <- post "/signin" signInRequest
  let rawToken = simpleBody response
  let token = fromJust (Aeson.decode rawToken)
  return token

loggedRequest :: DB.ByteString -> Method -> BSLU.ByteString -> WaiSession st SResponse
loggedRequest token method body = Test.Hspec.Wai.request method "/products" [("Authorization", "Bearer " <> token)] body

createUserRequest :: DBL.ByteString
createUserRequest = "{ \
\    \"name\": \"Andre\", \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

signInRequest :: DBL.ByteString
signInRequest = "{ \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

productListResponse :: TL.Text
productListResponse = "[{\"description\":\"just a table\",\"dimensions\":[{\"dimensionId\":1,\"images\":[{\"imageId\":2,\"url\":\"https://tiozao.co/image2.png\"},{\"imageId\":3,\"url\":\"https://tiozao.co/image3.png\"},{\"imageId\":4,\"url\":\"https://tiozao.co/image4.png\"}],\"name\":\"height\",\"productId\":1,\"symbol\":\"{height}\"},{\"dimensionId\":2,\"images\":[],\"name\":\"width\",\"productId\":1,\"symbol\":\"{width}\"},{\"dimensionId\":3,\"images\":[],\"name\":\"depth\",\"productId\":1,\"symbol\":\"{depth}\"}],\"images\":[{\"imageId\":1,\"url\":\"https://tiozao.co/image1.png\"}],\"name\":\"table\",\"priceFormula\":\"{height} * {width} * {depth}\",\"productId\":1,\"tag\":\"Product\"}]"

suiteSpec :: Connection -> Spec
suiteSpec dbConn = do

  with (api "127.0.0.1" "avm_test" "haskelluser" "haskellpassword" "secret2" 60) $ do
    describe "ProductsHandlerSpec" $ do

      it "Products return invalid Token" $ do
        liftIO (cleanDb dbConn)
        get "/products" `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "Should return empty product list" $ do
        liftIO (cleanDb dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodGet `shouldRespondWithPredicate` (\response -> "[]" == response, "List not empty")

      it "Should return product list" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodGet `shouldRespondWithPredicate` (\response -> productListResponse == response, "Expected response not fulfilled")

      it "Should calculate product price" $ do
        liftIO (cleanDb dbConn)
        liftIO (createProduct dbConn)
        token' <- createUser dbConn
        let token = BSL.toStrict (BSLU.fromString (Jwt.token token'))
        loggedRequest token methodPost `shouldRespondWithPredicate` (\response -> productListResponse == response, "Expected response not fulfilled")

{-
      it "LoggedHandler should fail with incorrect token" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        _ <- post "/signin" signInRequest
        let token = "incorrect token"
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] loggerHandlerRequest `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "LoggedHandler should fail if session is not found" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let rawToken = simpleBody response
        let token = fromJust (Aeson.decode rawToken)
        liftIO (execute dbConn "TRUNCATE TABLE sessions" ())
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict (BSLU.fromString (Jwt.token token)))] loggerHandlerRequest `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "LoggedHandler should fail if Json data is invalid" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let rawToken = simpleBody response
        let token = fromJust (Aeson.decode rawToken)
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict (BSLU.fromString (Jwt.token token)))] "Invalid data" `shouldRespondWith` "{\"code\":1,\"message\":\"Invalid Json format\"}" { matchStatus = 400 }

      it "LoggedHandler should return success" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let rawToken = simpleBody response
        let token = fromJust (Aeson.decode rawToken)
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict (BSLU.fromString (Jwt.token token)))] loggerHandlerRequest `shouldRespondWithPredicate` (\response -> "Session" `TL.isPrefixOf` response, "Start of the token does not match expected")

      it "LoggedHandler should fail because user does not have permission" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        response <- post "/signin" signInRequest
        let rawToken = simpleBody response
        let token = fromJust (Aeson.decode rawToken)
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict (BSLU.fromString (Jwt.token token)))] loggerHandlerRequest `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "LoggedHandler should fail with invalid json" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let token = simpleBody response
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] "invalid data" `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }
-}